"""
This module provides functions for generating private and public Diffie-Hellman keys,
as well as computing the shared secret key.
"""

import secrets


def private_key(prime_modulus):
    """
    Generates a private key for use in Diffie-Hellman key exchange.

    Args:
        prime_modulus (int): The prime modulus p.

    Returns:
        int: A randomly chosen private key.
    """
    return secrets.choice(range(2, prime_modulus))


def public_key(prime_modulus, generator, private_key):
    """
    Computes a public key for use in Diffie-Hellman key exchange.

    Args:
        prime_modulus (int): The prime modulus p.
        generator (int): The base generator g.
        private_key (int): The private key.

    Returns:
        int: The computed public key.
    """
    return (generator**private_key) % prime_modulus


def secret(prime_modulus, public_key, private_key):
    """
    Computes the shared secret using the private key and the peer's public key.

    Args:
        prime_modulus (int): The prime modulus p.
        public_key (int): The peer's public key.
        private_key (int): The private key.

    Returns:
        int: The computed shared secret.
    """
    return (public_key**private_key) % prime_modulus
