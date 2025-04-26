''' Add translation overlay for unknown words.'''
import asyncio
from gc import callbacks
import json
import os
import shutil
import tempfile
from difflib import Differ
import base64


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

async def translate(engine, text, from_code, to_code):
    cache = cache_translate(text, from_code, to_code)
    if cache:
        print('get cache')
        return cache
    if engine == "argos":
        return await argos_translate(text, from_code, to_code)
    elif engine == "mtranserver":
        return await mtranserver_translate(text, from_code, to_code)
    elif engine == "deepl":
        return await deepl_translate(text, from_code, to_code)
    else:
        print(f'{engine} not support.')

async def argos_translate(text, from_code, to_code):
    import argostranslate.package
    import argostranslate.translate
    result = argostranslate.translate.translate(text, from_code, to_code)
    return result

def cache_translate(text, from_code, to_code):
    cache_file = get_cache_file()
    with open(cache_file, "r") as file:
        data = json.load(file)
        if text in data and to_code in data[text]:
            return data[text][to_code]
    return None


def get_cache_file():
    global cache_directory
    if not os.path.exists(cache_directory):
        os.makedirs(cache_directory)
        print(f'Create cache directory: {cache_directory}')
    cache_file = os.path.join(cache_directory, 'real-time-translation.json')
    if not os.path.exists(cache_file):
        print(f'Create cache file: {cache_file}')
        with open(cache_file, 'w') as file:
            json.dump({}, file)
    return cache_file


def cache_translate_put(text, to_code, value):
    cache_file = get_cache_file()
    with open(cache_file, "r") as file:
        data = json.load(file)
        if text not in data:
            data[text] = {}
        data[text][to_code] = value
    with open(cache_file, "w") as file:
        json.dump(data, file)

async def mtranserver_translate(text, from_code, to_code):
    import requests
    global mtranserver_url
    url = mtranserver_url
    headers = {"Content-Type": "application/json"}
    data = {
        "from": from_code,
        "to": to_code,
        "text": text,
    }
    try:
        response = requests.post(url, headers=headers, json=data)
        response.raise_for_status()
        result = response.json()
        return result['result']
    except requests.exceptions.RequestException as e:
        print(f"Request Error: {e}")
        result = None

async def deepl_translate(text, from_code, to_code):
    import deepl
    global deepl_key
    deepl_client = deepl.DeepLClient(deepl_key)
    target_lang = get_deepl_lang_code(to_code)
    result = deepl_client.translate_text(text, target_lang=target_lang)
    cache_translate_put(text, to_code, result.text)
    return result.text

async def translate_text(text_info):

    text = text_info['text'].strip('\n')
    if is_empty_or_whitespace(text):
        return

    # Detect source language.
    language_info = detect(text)
    lang = language_info['lang']
    from_code = lang
    if target_languages[0] == lang:
        to_code = target_languages[1]
    else:
        to_code = target_languages[0]

    # high speed or high quality
    engine = high_speed_engine

    if 'high-quality' in text_info and text_info['high-quality']:
        engine = high_quality_engine

    translatedText = await translate(engine, text, from_code, to_code)
    refineText = None
    if refine_flag:
        refineText = await translate(engine, translatedText, to_code, from_code)

    del text_info['text']
    if refineText:
       text_info['refine'] = base64.b64encode(refineText.encode('utf-8')).decode('utf-8')
    if translatedText:
       text_info['trans'] = base64.b64encode(translatedText.encode('utf-8')).decode('utf-8')
    if refineText or translatedText:
        callback = text_info['callback']
        await run_in_emacs(callback, text_info)

async def translate_file(source_file, target_file):
    diff = diff_file(target_file, source_file)
    shutil.copyfile(source_file, target_file)
    # await translate_lines(diff)

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
        if cmd == "translate-text":
            print(info)
            text_info = info[1][1]
            await translate_text(text_info)
        else:
            print(f"not fount handler for {cmd}", flush=True)
    except:
        import traceback
        print(traceback.format_exc())



# eval in emacs and log the command.
async def run_and_log(cmd):
    print(cmd, flush=True)
    await bridge.eval_in_emacs(cmd)

def get_deepl_lang_code(lang):
    deepl_lang_map = {
        "bg": "BG",
        "cs": "CS",
        "da": "DA",
        "de": "DE",
        "el": "EL",
        "en": "EN-US",
        "es": "ES",
        "et": "ET",
        "fi": "FI",
        "fr": "FR",
        "hu": "HU",
        "id": "ID",
        "it": "IT",
        "ja": "JA",
        "lt": "LT",
        "lv": "LV",
        "nb": "NB",
        "nl": "NL",
        "pl": "PL",
        "pt-br": "PT-BR",
        "pt-pt": "PT-PT",
        "pt": "PT-PT",
        "ro": "RO",
        "ru": "RU",
        "sk": "SK",
        "sl": "SL",
        "sv": "SV",
        "tr": "TR",
        "zh": "ZH",
        "zh-cn": "ZH",
    }
    return deepl_lang_map.get(lang.lower())

def to_lisp_obj(obj):
    if not obj:
        return 'nil'
    if isinstance(obj, bool):
        if obj:
            return 't'
        return 'nil'
    if isinstance(obj, str):
        return f'\"{obj}\"'
    if isinstance(obj, int):
        return f'{obj}'
    if isinstance(obj, float):
        return f'{obj}'
    if isinstance(obj, list):
        return '(list ' + ' '.join([to_lisp_obj(item) for item in obj]) + ')'
    if isinstance(obj, dict):
        return '(list ' + ' '.join([f':{k} {to_lisp_obj(v)}' for (k, v) in obj.items()]) + ')'


async def run_in_emacs(func, arg):
    cmd = f'({func} {to_lisp_obj(arg)})'
    await run_and_log(cmd)

async def main():
    global bridge
    bridge = websocket_bridge_python.bridge_app_regist(on_message)
    await asyncio.gather(init(), bridge.start())

async def init():
    global target_languages, refine_flag, high_speed_engine, high_quality_engine, mtranserver_url, deepl_key, cache_directory
    print("init")
    target_languages = await get_emacs_var("real-time-translation-target-languages")
    refine_flag = await get_emacs_var("real-time-translation-refine-p")
    high_speed_engine = await get_emacs_var("real-time-translation-high-speed-engine")
    high_quality_engine = await get_emacs_var("real-time-translation-high-quality-engine")
    mtranserver_url = await get_emacs_var("real-time-translation-mtranserver-url")
    deepl_key = await get_emacs_var("real-time-translation-deepl-key")
    cache_directory = await get_emacs_var("real-time-translation-cache-directory")



async def get_emacs_var(var_name: str):
    "Get Emacs variable and format it."
    var_value = await bridge.get_emacs_var(var_name)
    var_value = json.loads(var_value)
    print(f'{var_name} : {var_value}')
    return var_value

asyncio.run(main())
