# -*- coding: utf-8 -*-

import weechat

SCRIPT_NAME    = "sdm-settings"
SCRIPT_AUTHOR  = "Samantha Marshall <hello@pewpewthespells.com>"
SCRIPT_VERSION = "0.1"
SCRIPT_LICENSE = "BSD-3"
SCRIPT_DESC    = "setting values back to specific channels"


settings = {
    'irc.server.freenode': {
        'notify': '2',
    },
    'irc.server.adelais': {
        'notify': '2',
    },
    'irc.server.cocoapods-slack': {
        'notify': '2',
    },
    'irc.freenode.##steam': {
        'notify': '3',
    },
    'irc.freenode.#macdev': {
        'notify': '3',
    },
    'irc.freenode.#OSXRE': {
        'notify': '3',
    },
    'irc.freenode.#python': {
        'notify': '1',
    },
    'irc.cocoapods-slack.#gripes': {
        'notify': '3',
    },
    'irc.cocoapods-slack.#cocoapods-app': {
        'notify': '3',
    },
    'irc.cocoapods-slack.#the-hivemind': {
        'notify': '3',
    },
    'irc.cocoapods-slack.#noise': {
        'notify': '3',
    },
    'irc.adelais.#podtacular': {},
    'irc.freenode.#machomebrew': {
        'notify': '3',
    },
    'irc.freenode.#weechat': {
        'notify': '1',
    },
}

def restore_buffer_settings(data, signal, signal_buffer): 
    buffer_name = weechat.buffer_get_string(signal_buffer, 'full_name')
    if buffer_name in list(settings.keys()):
        for key, value in settings[buffer_name].items():
            weechat.buffer_set(signal_buffer, key, value)
    
    return weechat.WEECHAT_RC_OK

if weechat.register(SCRIPT_NAME, SCRIPT_AUTHOR, SCRIPT_VERSION, SCRIPT_LICENSE, SCRIPT_DESC, "", ""):
    weechat.hook_signal('buffer_opened', 'restore_buffer_settings', '')
