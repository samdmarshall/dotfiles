#!/usr/bin/env bash
# script used for stripping the js file out for IE
lineone=`sed '/\<!--\[if lt IE 9\]\>/d' $1`
linetwo=`echo "$lineone" | sed '/\<script src=\"http\:\/\/html5shim\.googlecode\.com\/svn\/trunk\/html5.js\"\>\<\/script\>/d'`
linethree=`echo "$linetwo" | sed '/\<!\[endif\]--\>/d'`
echo "$linethree" > $1