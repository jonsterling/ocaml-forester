let hash source =
  Digest.to_hex @@ Digest.string source
