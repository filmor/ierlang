#!/bin/sh

escript rebar3 escriptize && python ../jupyter_kernel_test/test_ierl_kernel.py
