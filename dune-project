(lang dune 3.8)

(generate_opam_files true)
(opam_file_location inside_opam_directory)

(package
 (name ona)
 (synopsis "Shared library for Ona environment tools")
 (depends
  (ocaml (>= 5.0))
  (yojson (>= 2.0))))

(package
 (name ona-env-port-forward)
 (synopsis "Port forwarding tool for Ona environments with auto-reconnect")
 (depends
  (ocaml (>= 5.1))
  ona
  (cmdliner (>= 1.2))
  (yojson (>= 2.0))
  (minttea (>= 0.0.2))
  (spices (>= 0.0.2))))

(package
 (name ona-sync-zed-ssh-connections)
 (synopsis "A tool for syncing Zed ssh_connections settings with Ona environments")
 (depends
  (ocaml (>= 5.0))
  ona
  (cmdliner (>= 1.2))
  (yojson (>= 2.0))))
