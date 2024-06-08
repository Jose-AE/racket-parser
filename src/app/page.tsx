"use client";

import { useState } from "react";
import CodeMirror from "@uiw/react-codemirror";
import { vscodeDark } from "@uiw/codemirror-theme-vscode";
import axios from "axios";

export default function Component() {
  const [code, setCode] = useState("");
  const [parsedCodeHtml, setParsedCodeHtml] = useState("");

  const handleParse = () => {
    axios
      .post("/api/parse", { code })
      .then((res) => {
        console.log(res.data);
        setParsedCodeHtml(res.data.html);
      })
      .catch((err) => {
        console.log(err);
        setParsedCodeHtml(`<span>Syntax incorecto</span>`);
      });
  };

  return (
    <div className="h-screen bg-[rgba(51,51,51,255)] flex flex-col">
      <div className="h-[5%]  flex justify-center items-center">
        <button
          className="bg-blue-500  hover:bg-blue-700 text-white font-bold py-1 px-4 rounded"
          onClick={handleParse}
        >
          Parse
        </button>
      </div>

      <div className="flex h-[95%] p-3 gap-2">
        <div className="w-1/2 scrol overflow-y-auto bg-[rgba(30,30,30,255)] rounded-lg">
          <CodeMirror
            theme={vscodeDark}
            height="auto"
            value={code}
            onChange={(code) => {
              setCode(code);
            }}
            className="rounded-lg"
          />
        </div>
        <div
          dangerouslySetInnerHTML={{ __html: parsedCodeHtml }}
          className="text-slate-950 w-1/2 scrol overflow-y-auto p-2 bg-[rgb(232,229,229)] rounded-lg"
        ></div>
      </div>
    </div>
  );
}
