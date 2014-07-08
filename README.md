CryptoDiner
============
A Haskell implementation of [Chaum's dining cryptographers protocol](http://www.cs.cornell.edu/People/egs/herbivore/dcnets.html), using Z\*p Diffie-Hellman, for cryptographically secure anonymous communication.

Instead of using a sparse ring topology (vulnerable to deanonymization attacks with only N>=2 colluders) this implementation employs a slightly unorthodox public key broadcast protocol that constructs a complete key graph for every participant.

Roadmap:
======
- D-H key exchange ✓
- PRNG stream generation ✓
- Fast modular exponentiation ✓
- Testing ✓
- Proper testing
- First-round reservation negotiation
- Trap rounds to allow disrupters to be excluded from the network
- Key negotiation
