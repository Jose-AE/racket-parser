import * as fs from "fs";
import { exec } from "child_process";

function createTxtFile(filename: string, content: string): void {
  fs.writeFile(filename, content, (err) => {
    if (err) {
      console.error("Error occurred:", err);
      return;
    }
    console.log(`${filename} has been created successfully!`);
  });
}

export async function POST() {
  createTxtFile("input/test.txt", "hola");

  return Response.json({ data: "hello" });
}

export async function GET(req: Request) {
  let html = ``;

  exec("racket src/racket/parser.rkt", (error, stdout, stderr) => {
    if (error) {
      console.error(`Error executing Racket program: ${error}`);
      return;
    }
    if (stderr) {
      console.error(`Racket program encountered an error: ${stderr}`);
      return;
    }
    html += stdout;
    console.log(`Output from Racket program:\n${stdout}`);
  });

  return Response.json({ html });
}
