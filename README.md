# Contract Market

NFT Marketplace contract for swaps/collect with FA2 and FA12 tokens 

## Makefile usage

The repository provides a Makefile for compiling/testing/deploying the smart
contract. All makefile targets are described with the `make` command.

### Compile Shifumi contract

The repository provides a Makefile for compiling the smart contract.

```sh
make compile
```

You can also override `make` parameters by running :

```sh
make compile ligo_compiler=<LIGO_EXECUTABLE> protocol_opt="--protocol <PROTOCOL>"
```

It compiles the smart contract in TZ file and also in the JSON format

### Test contract

The repository provides a Makefile for testing the smart contract Shifumi.

```sh
make test
```

### Deploy contract

The repository provides a deployment script for deploying the smart contract.

```
make deploy
```

You have to rename `deploy/.env.dist` to `deploy/.env` and **fill the required variables** to be able to deploy.
