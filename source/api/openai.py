import os
from dotenv import load_dotenv
from openai import OpenAI
import json
import tkinter as tk
from tkinter import filedialog

load_dotenv()
client = OpenAI(
    api_key=os.getenv("OPENAPI_KEY")
)

root = tk.Tk()
root.withdraw()
file_path = filedialog.askopenfilename()
print(file_path)
f = open(file_path, "r")
text = f.read()

file = open("instruction.txt")
instruction = file.read()


response = client.responses.create(

    model="gpt-4o-mini",
    input=[
        {
            "role": "user",
            "content": [
                {
                    "type": "input_text",
                    "text": instruction+"\n"+text
                }
            ]
        }
    ],
    reasoning={},
    store=True
)

print(response.output_text)

fileName = file_path.split("/")[-1]
with open("outputFiles/"+fileName+".md", "w") as outputFile:
    outputFile.write(response.output_text)
f.close()