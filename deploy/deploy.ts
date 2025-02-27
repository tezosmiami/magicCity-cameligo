import { InMemorySigner } from "@taquito/signer";
import { MichelsonMap, TezosToolkit} from "@taquito/taquito";
import { buf2hex } from "@taquito/utils";
import chalk from "chalk";
import { Spinner } from "cli-spinner";
import * as dotenv from "dotenv";
import code from "../compiled/market.json";

dotenv.config({ path: __dirname + "/.env" });

const missingEnvVarLog = (name: string) =>
  console.log(
    chalk.redBright`Missing ` +
      chalk.red.bold.underline(name) +
      chalk.redBright` env var. Please add it in ` +
      chalk.red.bold.underline(`deploy/.env`)
  );

const makeSpinnerOperation = async <T>(
  operation: Promise<T>,
  {
    loadingMessage,
    endMessage,
  }: {
    loadingMessage: string;
    endMessage: string;
  }
): Promise<T> => {
  const spinner = new Spinner(loadingMessage);
  spinner.start();
  const result = await operation;
  spinner.stop();
  console.log("");
  console.log(endMessage);

  return result;
};

const rpcUrl = process.env.RPC_URL; //"http://127.0.0.1:8732"
const pk = process.env.PK;

if (!pk && !rpcUrl) {
  console.log(
    chalk.redBright`Couldn't find env variables. Have you renamed ` +
      chalk.red.bold.underline`deploy/.env.dist` +
      chalk.redBright` to ` +
      chalk.red.bold.underline(`deploy/.env`)
  );

  process.exit(-1);
}

if (!pk) {
  missingEnvVarLog("PK");
  process.exit(-1);
}

if (!rpcUrl) {
  missingEnvVarLog("RPC_URL");
  process.exit(-1);
}

const Tezos = new TezosToolkit(rpcUrl);
const signer = new InMemorySigner(pk);
Tezos.setProvider({ signer: signer });

  async function deploy() {
    let storage = {
      metadata: MichelsonMap.fromLiteral({
        '':
        buf2hex(Buffer.from("ipfs://QmRhaKzkHXRWvAtW8mxEmztHywdn2oQ9TApxQ4Yq2BqdPN")),
      }),
      next_id: 0,
      swaps: new MichelsonMap(),
      fa2s: [],
      currencies: new MichelsonMap(),
      admin: 'tz1dZTjhDDhFcVGiXY1WmGuDSrstRNW9Hna4',
      pending_admin: null,
      fee: 25,
      paused: false,
  
    };

  try {
    const origination = await makeSpinnerOperation(
      Tezos.contract.originate({
        code,
        storage,
      }),
      {
        loadingMessage: chalk.yellowBright`Deploying contract`,
        endMessage: chalk.green`Contract deployed!`,
      }
    );

    await makeSpinnerOperation(origination.contract(), {
      loadingMessage:
        chalk.yellowBright`Waiting for contract to be confirmed at: ` +
        chalk.yellow.bold(origination.contractAddress),
      endMessage: chalk.green`Contract confirmed!`,
    });

    console.log(
      chalk.green`\nContract address: \n- ` +
        chalk.green.underline`${origination.contractAddress}`
    );
  } catch (error: any) {
    console.log("");
    console.log(chalk.redBright`Error during deployment:`);
    console.log(error);
    process.exit(1);
  }
}

deploy();
