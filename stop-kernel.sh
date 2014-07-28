#!/bin/bash

JAPIPID=`cat JAPI.pid`
kill $JAPIPID
rm JAPI.pid
