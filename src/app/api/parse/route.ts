import * as fs from "fs";
import { exec } from "child_process";
import { promisify } from "util";

const execAsync = promisify(exec);
const writeFileAsync = promisify(fs.writeFile);

export async function POST(req: Request) {
  const { code } = await req.json();

  try {
    await writeFileAsync("./src/racket/input/source_code.txt", code);
    console.log(`Source code has been created successfully!`);
  } catch (error) {
    console.error("Error occurred:", error);
  }

  try {
    const { stdout, stderr } = await execAsync("racket src/racket/parser.rkt");

    if (stderr) {
      console.error(`Racket program encountered an error: ${stderr}`);
      return Response.json({ error: stderr }, { status: 500 });
    }

    //console.log(`Output from Racket program:\n${stdout}`);
    return Response.json({ html: stdout.replace("\n", "") });
  } catch (error: any) {
    console.error(`Error executing Racket program: ${error}`);
    return Response.json({ error: error.message }, { status: 500 });
  }
}
