{
  description = "ACAP+Rust development environment";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/25.11";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    {
      self,
      nixpkgs,
      rust-overlay,
    }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        config.allowUnfree = true;
        inherit system;
        overlays = [ rust-overlay.overlays.default ];
      };

      # SDK version to use
      sdkVersion = "12.1.0";

      # Import SDK packages
      mkhelp = pkgs.callPackage ./pkgs/mkhelp { };
      acap-sdk-aarch64 = pkgs.callPackage ./pkgs/acap-native-sdk { version = sdkVersion; arch = "aarch64"; };
      acap-sdk-armv7hf = pkgs.callPackage ./pkgs/acap-native-sdk { version = sdkVersion; arch = "armv7hf"; };

      # Cross-compilation toolchains
      aarch64-cc = pkgs.pkgsCross.aarch64-multiplatform.stdenv.cc;
      armv7hf-cc = pkgs.pkgsCross.armv7l-hf-multiplatform.stdenv.cc;

      rustToolchain = pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;
    in
    {
      devShells.${system}.default = pkgs.mkShellNoCC {
        buildInputs = with pkgs; [
          # Libraries for native builds
          openssl
          glib
          cairo
        ];

        nativeBuildInputs = with pkgs; [
          # Rust toolchain
          rustToolchain

          # Cross-compilation toolchains
          pkgsCross.aarch64-multiplatform.stdenv.cc
          pkgsCross.armv7l-hf-multiplatform.stdenv.cc

          # Build tools
          clang
          pkg-config
          llvmPackages.libclang.lib

          # Utilities
          curl
          git
          sshpass
          iputils
        ];

        packages = with pkgs; [
          fd
          mkhelp
          nixfmt-rfc-style
        ];

        shellHook = ''
          # SDK sysroot paths
          export SYSROOT_AARCH64="${acap-sdk-aarch64.sysroot}"
          export SYSROOT_ARMV7HF="${acap-sdk-armv7hf.sysroot}"

          # Cargo cross-compilation for aarch64
          export CARGO_TARGET_AARCH64_UNKNOWN_LINUX_GNU_LINKER="${aarch64-cc}/bin/${aarch64-cc.targetPrefix}gcc"
          export CARGO_TARGET_AARCH64_UNKNOWN_LINUX_GNU_RUSTFLAGS="-C link-args=--sysroot=$SYSROOT_AARCH64"
          export CC_aarch64_unknown_linux_gnu="${aarch64-cc}/bin/${aarch64-cc.targetPrefix}gcc"
          export CXX_aarch64_unknown_linux_gnu="${aarch64-cc}/bin/${aarch64-cc.targetPrefix}g++"
          export PKG_CONFIG_LIBDIR_aarch64_unknown_linux_gnu="$SYSROOT_AARCH64/lib/pkgconfig:$SYSROOT_AARCH64/share/pkgconfig"
          export PKG_CONFIG_PATH_aarch64_unknown_linux_gnu="$SYSROOT_AARCH64/lib/pkgconfig:$SYSROOT_AARCH64/share/pkgconfig"
          export PKG_CONFIG_SYSROOT_DIR_aarch64_unknown_linux_gnu="$SYSROOT_AARCH64"

          # Cargo cross-compilation for armv7hf (thumbv7neon)
          export CARGO_TARGET_THUMBV7NEON_UNKNOWN_LINUX_GNUEABIHF_LINKER="${armv7hf-cc}/bin/${armv7hf-cc.targetPrefix}gcc"
          export CARGO_TARGET_THUMBV7NEON_UNKNOWN_LINUX_GNUEABIHF_RUSTFLAGS="-C link-args=--sysroot=$SYSROOT_ARMV7HF"
          export CC_thumbv7neon_unknown_linux_gnueabihf="${armv7hf-cc}/bin/${armv7hf-cc.targetPrefix}gcc"
          export CXX_thumbv7neon_unknown_linux_gnueabihf="${armv7hf-cc}/bin/${armv7hf-cc.targetPrefix}g++"
          export PKG_CONFIG_LIBDIR_thumbv7neon_unknown_linux_gnueabihf="$SYSROOT_ARMV7HF/lib/pkgconfig:$SYSROOT_ARMV7HF/share/pkgconfig"
          export PKG_CONFIG_PATH_thumbv7neon_unknown_linux_gnueabihf="$SYSROOT_ARMV7HF/lib/pkgconfig:$SYSROOT_ARMV7HF/share/pkgconfig"
          export PKG_CONFIG_SYSROOT_DIR_thumbv7neon_unknown_linux_gnueabihf="$SYSROOT_ARMV7HF"
          export BINDGEN_EXTRA_CLANG_ARGS_thumbv7neon_unknown_linux_gnueabihf="--sysroot=$SYSROOT_ARMV7HF -I$SYSROOT_ARMV7HF/include"
          export CFLAGS_thumbv7neon_unknown_linux_gnueabihf="--sysroot=$SYSROOT_ARMV7HF"

          # Other environment setup
          export LIBCLANG_PATH="${pkgs.llvmPackages.libclang.lib}/lib"
          export RUST_SRC_PATH="${rustToolchain}/lib/rustlib/src/rust/library"

          # Prevent cargo from finding programs in the default cargo home
          export PATH="$PATH:$HOME/.cargo/bin"

          echo "ACAP SDK ${sdkVersion} development environment loaded"
          echo "  - aarch64 sysroot: $SYSROOT_AARCH64"
          echo "  - armv7hf sysroot: $SYSROOT_ARMV7HF"
        '';
      };
    };
}
