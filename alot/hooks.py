#!/usr/bin/env python3

#=========
# Imports
#=========


# import os
# import datetime
# import logging
# import asyncio
# import subprocess

# from alot.settings.const import settings
# from alot.ui import UI
# from alot.commands import Command, registerCommand


#================
# Hook Functions
#================

#
# Hooks are defined as `order`_`mode`_`command`,
#
#   `order` = pre/post
#
#   `mode` = global, bufferlist, envelope, namedqueries, search, taglist, thread
#
#   `command` = https://alot.readthedocs.io/en/latest/usage/index.html#usage-commands
#

# _preferred_account = None

# Called via `call hooks.startup` in the config file.
# async def startup(ui=None):
#   logging.info("starting up...")
#   accounts = settings.get_accounts()
#   total = len(accounts)
#   if total == 0:
#     logging.fatal("Count not find any accounts in config file!!")
#   elif total == 1:
#     logging.info("Selecting default account...")
#     _preferred_account = accounts[0]
#   else:
#     account_selections = dict((str(accounts.index(account)), str(account.address)) for account in accounts)
#     address = await UI.choice(ui, "Select account", choices=account_selections)
#     _preferred_account = settings.account_matching_address(address)
#   logging.info(ui.current_buffer.querystring)

# Called prior to the application exiting.
# def pre_global_exit(**kwargs):
#   logging.info("closing now...")

# Called periodically (`periodic_hook_frequency` = [300 sec, default]) while application is in use.
# def pre_global_loop_hook(**kwargs):
#   logging.info("idle update...")

# @registerCommand(MODE, 'fetch')
# class FetchCommand(Command):

#   repeatable = True

#   async def apply(self, ui):
#     command = ["notmuch", "new"]

#     config_dir = os.path.dirname(settings.hooks.__file__)
#     logging.info(config_dir)

#     logging_dir = os.path.join(config_dir, "logs/")
#     logging.info(logging_dir)

#     if not (os.path.exists(logging_dir) and os.path.isdir(logging_dir)):
#       os.makedirs(logging_dir)

#     time = datetime.date.today().strftime("%Y%m%d %H%M%S")
#     log_file_path = "_".join(command) + " " + time

#     stdout_log_path = os.path.join(logging_dir, log_file_path + ".stdout.log")
#     stderr_log_path = os.path.join(logging_dir, log_file_path + ".stderr.log")

#     stdout_log = open(stdout_log_path, 'w')
#     stderr_log = open(stderr_log_path, 'w')

#     result = await subprocess.Popen(command, stdout=stdout_log, stderr=stderr_log)
#     return result
