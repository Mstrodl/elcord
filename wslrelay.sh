#!/usr/bin/env bash

### fix wsl2 interop
### https://github.com/microsoft/WSL/issues/5065#issuecomment-682198412
for i in $(pstree -np -s $$ | grep -o -E '[0-9]+'); do
    if [[ -e "/run/WSL/${i}_interop" ]]; then
        export WSL_INTEROP=/run/WSL/${i}_interop
    fi
done

exec npiperelay.exe -ep -s //./pipe/$1
