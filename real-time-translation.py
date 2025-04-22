''' Add translation overlay for unknown words.'''
import asyncio
import json
import os
import re
import shutil
import tempfile
from difflib import Differ

import argostranslate.package
import argostranslate.translate
import websocket_bridge_python

from fast_langdetect import (
    detect,
)
from sexpdata import dumps

from_code = "zh"
to_code = "en"


file_map = {}


def diff_file(file1, file2):
    with open(file1) as file_1, open(file2) as file_2:
        differ = Differ()
        diff = {}
        lineNum = 0
        for line in differ.compare(file_1.readlines(), file_2.readlines()):
            code = line[:2]
            if code in ("  ", "+ "):
                lineNum += 1
            if code == "+ ":
                diff[lineNum] = line[2:].strip()
        return diff

async def translate_lines(lines):
    global target_languages
    newlines = {}
    for key in lines:
        text = lines[key].rstrip('\n')
        if is_empty_or_whitespace(text):
            continue
        language_info = detect(text)
        lang = language_info['lang']
        from_code = lang
        if target_languages[0] == lang:
            to_code = target_languages[1]
        else:
            to_code = target_languages[0]

        translatedText = argostranslate.translate.translate(text, from_code, to_code)
        refineText = argostranslate.translate.translate(translatedText, to_code, from_code)

        newlines[str(key)] = f'{translatedText}\n{refineText}'
        await run_and_log(f'(real-time-translation-render \'{dumps(newlines)})')


async def translate_file(source_file, target_file):
    diff = diff_file(target_file, source_file)
    shutil.copyfile(source_file, target_file)
    await translate_lines(diff)

def is_empty_or_whitespace(s):
    # If the character is empty or all characters are blank, return True.
    return not s or s.isspace()

async def translate_diff_file(file_name):

    if file_name in file_map:
        await translate_file(file_name, file_map[file_name])
    else:

        f = tempfile.NamedTemporaryFile(delete=False, suffix="-" + os.path.basename(file_name))
        file_map[file_name] = f.name
        await translate_file(file_name, f.name)


# dispatch message received from Emacs.
async def on_message(message):
    try:
        info = json.loads(message)
        cmd = info[1][0].strip()
        if cmd == "translate":
            file_name = info[1][1]
            await translate_diff_file(file_name)
        elif cmd == "translate-line":
            line_num = info[1][1]
            line_str = info[1][2]
            await translate_lines({line_num: line_str})
        else:
            print(f"not fount handler for {cmd}", flush=True)
    except:
        import traceback
        print(traceback.format_exc())



# eval in emacs and log the command.
async def run_and_log(cmd):
    print(cmd, flush=True)
    await bridge.eval_in_emacs(cmd)

async def main():
    global bridge
    bridge = websocket_bridge_python.bridge_app_regist(on_message)
    await asyncio.gather(init(), bridge.start())

async def init():
    global target_languages
    print("init")
    target_languages = await get_emacs_var("real-time-translation-target-languages")

async def get_emacs_var(var_name: str):
    "Get Emacs variable and format it."
    var_value = await bridge.get_emacs_var(var_name)
    var_value = json.loads(var_value)
    print(f'{var_name} : {var_value}')
    return var_value

asyncio.run(main())
