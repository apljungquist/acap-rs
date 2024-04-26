use std::io::ErrorKind::{AlreadyExists, NotFound};
use std::os::unix::fs::PermissionsExt;
use std::path::Path;
use std::{convert::Infallible, fs, io};

use axum::http::Request;
use axum::routing::IntoMakeService;
use axum::Router;
use hyper::body::Incoming;
use hyper_util::{
    rt::{TokioExecutor, TokioIo},
    server,
};
use log::{debug, error};
use tokio::net::{unix::SocketAddr, UnixListener, UnixStream};
use tower::Service;

fn unwrap_infallible<T>(result: Result<T, Infallible>) -> T {
    match result {
        Ok(t) => t,
        Err(e) => match e {},
    }
}

pub fn listener(sock: &Path) -> anyhow::Result<UnixListener> {
    match fs::remove_file(sock) {
        Ok(()) => {}
        Err(e) if e.kind() == NotFound => {}
        e => e?,
    }
    let uds = UnixListener::bind(sock)?;
    fs::set_permissions(sock, fs::Permissions::from_mode(0o666))?;
    Ok(uds)
}

async fn accept(
    conn: io::Result<(UnixStream, SocketAddr)>,
    make_service: &mut IntoMakeService<Router>,
) {
    let stream = match conn {
        Ok((stream, _)) => stream,
        Err(e) => {
            error!("Could not accept connection because {e:?}");
            return;
        }
    };
    let local_addr = stream
        .local_addr()
        .unwrap()
        .as_pathname()
        .unwrap()
        .to_path_buf();
    debug!("Accepted connection to {local_addr:?}");

    let router = unwrap_infallible(make_service.call(&stream).await);

    tokio::spawn(async move {
        let stream = TokioIo::new(stream);
        let service = hyper::service::service_fn(move |request: Request<Incoming>| {
            router.clone().call(request)
        });
        match server::conn::auto::Builder::new(TokioExecutor::new())
            .serve_connection_with_upgrades(stream, service)
            .await
        {
            Ok(()) => {}
            Err(e) => {
                error!("Could not serve connection because {e}");
            }
        }

        debug!("Closed connection {local_addr:?}")
    });
}

pub async fn serve(app: Router, dir: &Path) -> anyhow::Result<()> {
    let mut make_service = app.into_make_service();

    match fs::create_dir(dir) {
        Ok(()) => {}
        Err(e) if e.kind() == AlreadyExists => {}
        e => e?,
    }
    // This is not enough to prevent other users from creating the sockets,
    // but it does shrink the window of opportunity.
    fs::set_permissions(dir, fs::Permissions::from_mode(0o711))?;
    // Note that any user on the system can connect to these sockets.
    let admin_sock = listener(&dir.join("admin.socket"))?;
    let operator_sock = listener(&dir.join("operator.socket"))?;
    let viewer_sock = listener(&dir.join("viewer.socket"))?;
    loop {
        debug!("Waiting to accept connection...");
        tokio::select! {
            conn = admin_sock.accept() => { accept(conn, &mut make_service).await}
            conn = operator_sock.accept() => { accept(conn, &mut make_service).await}
            conn = viewer_sock.accept() => { accept(conn, &mut make_service).await}
        }
    }
}
