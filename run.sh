#!/bin/bash

fsharpc --target:exe --out:Turtle.exe --optimize+ --tailcalls+ Turtle.fs && mono Turtle.exe