{ mkDerivation, base, base64-bytestring, blaze-html, byteable
, bytestring, cryptohash, HsOpenSSL, http-streams, stdenv, tagsoup
, time
}:
mkDerivation {
  pname = "ses-html";
  version = "0.4.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base base64-bytestring blaze-html byteable bytestring cryptohash
    HsOpenSSL http-streams tagsoup time
  ];
  description = "Send HTML formatted emails using Amazon's SES REST API with blaze";
  license = stdenv.lib.licenses.bsd3;
}
