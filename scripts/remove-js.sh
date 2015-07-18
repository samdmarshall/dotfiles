#!/usr/bin/env bash
# script used for stripping the js file out for IE
lineone=`sed '/\<!--\[if lt IE 9\]\>/d' $1`
linetwo=`echo "$lineone" | sed '/\<script src=\"http\:\/\/html5shim\.googlecode\.com\/svn\/trunk\/html5.js\"\>\<\/script\>/d'`
linethree=`echo "$linetwo" | sed '/\<!\[endif\]--\>/d'`
linefour=`echo "$linethree" | sed '/\<style type=\"text\/css\"\>code{white-space\: pre;}\<\/style\>/d'`
linefive=`echo "$linefour" | sed '/\<style type=\"text\/css\"\>\<\/style\>/d'`
echo "$linefive" > $1