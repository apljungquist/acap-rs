//! An example of how to run a webserver and expose it using reverse proxy and unix domain socket

use std::path::PathBuf;

use anyhow::bail;
use axum::extract::Path;
use axum::routing::get;
use axum::Router;
use log::debug;
use serde::Deserialize;
use tower_http::trace::DefaultMakeSpan;
use tower_http::trace::TraceLayer;

mod app_logging;
mod tcp;
mod uds;

const ACAP_NAME: &str = "hello_reverse_proxy";

#[derive(Debug, Deserialize)]
#[serde(rename_all = "snake_case")]
enum Group {
    Admin,
    Operator,
    Viewer,
}

async fn hello_authenticated_user(Path(group): Path<Group>) -> String {
    format!("Hello {group:?}")
}

fn new_app() -> Router {
    Router::new()
        .route(
            &format!("/local/{ACAP_NAME}/api/:group"),
            get(hello_authenticated_user),
        )
        .layer(
            TraceLayer::new_for_http().make_span_with(DefaultMakeSpan::new().include_headers(true)),
        )
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    app_logging::init_logger();
    let app = new_app();

    match std::env::var("TCP_PORT") {
        Ok(port) => {
            debug!("Serving using TCP");
            tcp::serve(app, &format!("127.0.0.1:{port}")).await?
        }
        Err(std::env::VarError::NotPresent) => {
            debug!("Serving using UDS");
            uds::serve(app, &PathBuf::from(format!("/run/http/{ACAP_NAME}"))).await?;
        }
        Err(std::env::VarError::NotUnicode(e)) => {
            bail!("{e:?} is not a valid unicode string");
        }
    }
    Ok(())
}
