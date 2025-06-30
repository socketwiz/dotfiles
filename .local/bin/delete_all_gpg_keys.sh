#!/bin/bash

echo "⚠️  WARNING: This will DELETE ALL GPG KEYS (public and private) from your keyring."
read -p "Are you sure you want to proceed? [y/N] " confirm
if [[ "$confirm" != [yY] ]]; then
  echo "❌ Aborted."
  exit 1
fi

echo "🔐 Deleting secret keys..."
gpg --list-secret-keys --with-colons \
  | awk -F: '/^fpr:/ { print $10 }' \
  | while read -r fingerprint; do
    echo "🗑️  Deleting secret key: $fingerprint"
    gpg --batch --yes --delete-secret-keys "$fingerprint"
done

echo "🔓 Deleting public keys..."
gpg --list-keys --with-colons \
  | awk -F: '/^fpr:/ { print $10 }' \
  | while read -r fingerprint; do
    echo "🗑️  Deleting public key: $fingerprint"
    gpg --batch --yes --delete-keys "$fingerprint"
done

echo "✅ All GPG keys deleted."

