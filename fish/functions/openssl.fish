function certinfo --wraps=openssl
  command openssl x509 -inform DER -text -in $argv
end
