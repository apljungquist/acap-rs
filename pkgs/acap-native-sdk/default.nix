{
  lib,
  dockerTools,
  runCommand,
  jq,
  # Version and architecture parameters
  version ? "12.1.0",
  arch ? "aarch64",
  # Override parameters (optional)
  imageDigest ? null,
  imageSha256 ? null,
  envSetupScript ? null,
  targetSysroot ? null,
}:

assert imageDigest != null -> imageSha256 != null;

let
  # Constants
  repo = "axisecp";
  sdk = "acap-native-sdk";

  # Version-specific defaults
  versionDefaults = {
    "12.1.0" = {
      ubuntuVersion = "24.04";
      archs = {
        aarch64 = {
          imageDigest = "sha256:59fbff8af293c253db76b7d3d5703af6eef34c9c32c7210f5f4a1fababfaa516";
          imageSha256 = "sha256-i+3DnKVbPZNxUUjPUvCAa64V8PGWTC/GeRCsX6DI9tU=";
          envSetupScript = "environment-setup-cortexa53-crypto-poky-linux";
          targetSysroot = "opt/axis/acapsdk/sysroots/aarch64";
        };
        armv7hf = {
          imageDigest = "sha256:b39c18c81bb7cb5cca04833a1f2e29f559b75171d79cd296a416a3b44899539f";
          imageSha256 = "sha256-P6sERcnCiiraNGd+i3jb5LQR9asksYnN7e8uxtGC9Aw=";
          envSetupScript = "environment-setup-cortexa9hf-neon-poky-linux-gnueabi";
          targetSysroot = "opt/axis/acapsdk/sysroots/armv7hf";
        };
      };
    };
    "12.7.0" = {
      ubuntuVersion = "24.04";
      archs = {
        aarch64 = {
          imageDigest = "sha256:5b816770663e85f402fedaa9c89d2353e1e4e5fe359ae67bda70ff4d63a39e89";
          imageSha256 = "sha256-muvKHz53RjtDMoNrIVP7mruEmpweaddF5JE/3LXKdGw=";
          envSetupScript = "environment-setup-cortexa53-crypto-poky-linux";
          targetSysroot = "opt/axis/acapsdk/sysroots/aarch64";
        };
        armv7hf = {
          imageDigest = "sha256:137cc78820f843777d6943ae6f96c4253e089d085c378c712fab2977a4161834";
          imageSha256 = "sha256-ajhKprjJ57EolyEBNzj/gSI1zz8p7DJF3yhxq6eJPYs=";
          envSetupScript = "environment-setup-cortexa9hf-neon-poky-linux-gnueabi";
          targetSysroot = "opt/axis/acapsdk/sysroots/armv7hf";
        };
      };
    };
  };

  versionConfig = versionDefaults.${version} or (throw "Unsupported SDK version: ${version}. Supported versions: ${lib.concatStringsSep ", " (lib.attrNames versionDefaults)}");
  archConfig = versionConfig.archs.${arch} or (throw "Unsupported architecture: ${arch} for version ${version}. Supported architectures: ${lib.concatStringsSep ", " (lib.attrNames versionConfig.archs)}");

  # Construct image tag
  imageTag = "${version}-${arch}-ubuntu${versionConfig.ubuntuVersion}";

  # Pull the Docker image
  image = dockerTools.pullImage {
    imageName = "${repo}/${sdk}";
    imageDigest = if imageDigest != null then imageDigest else archConfig.imageDigest;
    sha256 = if imageSha256 != null then imageSha256 else archConfig.imageSha256;
    finalImageTag = imageTag;
    finalImageName = "${repo}/${sdk}";
  };

  actualEnvSetupScript = if envSetupScript != null then envSetupScript else archConfig.envSetupScript;
  actualTargetSysroot = if targetSysroot != null then targetSysroot else archConfig.targetSysroot;

  # Base unpacked image
  unpacked = runCommand "acap-native-sdk-${version}-unpacked-${arch}"
    {
      nativeBuildInputs = [ jq ];
      inherit image;
    }
    ''
      mkdir -p $out
      workdir=$(mktemp -d)

      # Extract the Docker tar archive
      echo "Extracting Docker image archive..."
      tar -xf ${image} -C $workdir

      # Read the manifest to find layer order
      manifest=$(cat $workdir/manifest.json | jq -r '.[0]')

      # Extract each layer in order and handle whiteout files
      echo "$manifest" | jq -r '.Layers[]' | while read layer; do
        echo "Extracting layer: $layer"
        # Make all directories writable so tar can add files to them
        chmod -R u+w $out 2>/dev/null || true
        # Use --no-same-owner and ignore errors (permission issues are OK in sandbox)
        tar -xf "$workdir/$layer" -C $out --no-same-owner 2>/dev/null || true

        # Handle Docker whiteout files (.wh.filename means delete filename)
        find $out -name '.wh.*' -type f 2>/dev/null | while read whiteout; do
          dir=$(dirname "$whiteout")
          file=$(basename "$whiteout")
          # Remove .wh. prefix to get the actual filename to delete
          target="$dir/''${file#.wh.}"
          echo "Processing whiteout: $whiteout -> deleting $target"
          chmod u+w "$dir" 2>/dev/null || true
          rm -rf "$target" 2>/dev/null || true
          rm -f "$whiteout" 2>/dev/null || true
        done 2>/dev/null || true
      done

      # Remove any remaining whiteout files and empty directories
      find $out -name '.wh.*' -delete 2>/dev/null || true
      find $out -type d -empty -delete 2>/dev/null || true
    '';

  # Package containing just the target sysroot
  sysroot = runCommand "acap-native-sdk-${version}-sysroot-${arch}"
    { }
    ''
      mkdir -p $out
      cp -r ${unpacked}/${actualTargetSysroot}/* $out/
    '';

  # Package containing just the environment setup script
  envSetup = runCommand "acap-native-sdk-${version}-env-setup-${arch}"
    { }
    ''
      mkdir -p $out
      cp ${unpacked}/opt/axis/acapsdk/${actualEnvSetupScript} $out/${actualEnvSetupScript}
    '';

  # Package containing just the manifest tools
  manifestTools = runCommand "acap-native-sdk-${version}-manifest-tools-${arch}"
    { }
    ''
      mkdir -p $out
      cp -r ${unpacked}/opt/axis/acapsdk/axis-acap-manifest-tools/* $out/
    '';

  # Package containing the x86_64 host/native sysroot
  hostSysroot = runCommand "acap-native-sdk-${version}-host-sysroot-${arch}"
    { }
    ''
      mkdir -p $out
      cp -r ${unpacked}/opt/axis/acapsdk/sysroots/x86_64-pokysdk-linux/* $out/
    '';
in
{
  inherit sysroot envSetup manifestTools hostSysroot;
  full = unpacked;

  meta = {
    description = "ACAP Native SDK Docker image (unpacked)";
    homepage = "https://hub.docker.com/r/axisecp/acap-native-sdk";
    license = lib.licenses.unfree;
  };
}
