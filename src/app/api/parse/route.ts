import { promises as fs } from "fs";
import { exec } from "child_process";
import { promisify } from "util";

const execAsync = promisify(exec);

const RACKET_PATH = "./src/racket";

export async function POST(req: Request) {
  const { code } = await req.json();

  //create source code.txt from request
  try {
    await fs.writeFile(`${RACKET_PATH}/input/source_code.txt`, code);
    console.log(`Source code has been created successfully!`);
  } catch (error: any) {
    console.error("Error occurred:", error);
    return Response.json({ error: error.message }, { status: 500 });
  }

  //Run racket parser
  try {
    const { stdout, stderr } = await execAsync(`racket ${RACKET_PATH}/parser.rkt`);

    if (stderr) {
      console.error(`Racket program encountered an error: ${stderr}`);
    }
    //console.log(`Output from Racket program:\n${stdout}`);
  } catch (error: any) {
    console.error(`Error executing Racket program: ${error}`);
    return Response.json({ error: error.message }, { status: 500 });
  }

  //Return racket parsed html file
  try {
    const html = await fs.readFile(
      `${RACKET_PATH}/output/parsed_code.html`,
      "utf8"
    );
    //    console.log(html.);
    return Response.json({ html });
  } catch (error: any) {
    console.error(`Error reading parsed_code.html: ${error}`);
    return Response.json({ error: error.message }, { status: 500 });
  }
}
