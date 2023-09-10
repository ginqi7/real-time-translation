''' Add translation overlay for unknown words.'''
import asyncio
import json
import os
import re
import shutil
import tempfile
from sys import platform
from threading import Timer
from difflib import Differ

import websocket_bridge_python
from sexpdata import dumps


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
    print(lines)
    for key in lines:
        import google_translate
        result = google_translate.translate(lines[key], dst_lang='en')
        lines[key] ="".join(result["trans"])
        
    await run_and_log(f'(real-time-translation-render \'{dumps(lines)})')


async def translate_file(source_file, target_file):
    diff = diff_file(target_file, source_file)
    await translate_lines(diff)
    print(diff)
    shutil.copyfile(source_file, target_file)




async def translate_diff_file(file_name):
    if file_name in file_map:
        await translate_file(file_name, file_map[file_name])
    else:
        f = tempfile.NamedTemporaryFile(delete=False, suffix="-" + os.path.basename(file_name))
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
    print("init")
    
async def get_emacs_var(var_name: str):
    "Get Emacs variable and format it."
    var_value = await bridge.get_emacs_var(var_name)
    if isinstance(var_value, str):
        var_value = var_value.strip('"')
    print(f'{var_name} : {var_value}')
    if var_value == 'null':
        return None
    return var_value
    
asyncio.run(main())
