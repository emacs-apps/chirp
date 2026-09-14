# Third-party notices for the optional XChat module

The exact dependency graph is recorded in `chirp-xchat-module/Cargo.lock`. Chirp distributes the module and the required `chat-xdk` and Juicebox SDK source snapshots. Native binary redistribution is not supported and must wait for a complete generated attribution bundle covering every locked crate.

- X `chat-xdk` 0.5.0, commit `017650c0dec4018e282e878c52a5874abb911b34`, is MIT-licensed. See `vendor/chat-xdk/LICENSE` and its upstream inventory in `vendor/chat-xdk/THIRD_PARTY_NOTICES.md`.
- Juicebox SDK 0.3.7, commit `2e0f544e3fd0bd0997a034f73c8efb6fade3690c`, is MIT-licensed. See `vendor/juicebox-sdk/LICENSE`.
- Apache Thrift 0.24.0 from crates.io is Apache-2.0-licensed. See `licenses/apache-thrift-APACHE-2.0.txt` and `licenses/apache-thrift-NOTICE.txt`.
- `emacs`, `emacs-macros`, and `emacs_module` 0.21.0 are BSD-3-Clause-licensed. See `licenses/emacs-module-rs-BSD-3-Clause.txt`.

The files below record the audited top-level sources for development builds; they are not yet a complete binary-distribution notice bundle.
