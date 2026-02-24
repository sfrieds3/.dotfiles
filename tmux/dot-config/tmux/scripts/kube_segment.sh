#!/bin/sh

set -eu

ttl="${1:-20}"
cache_file="${TMPDIR:-/tmp}/tmux-kube-${UID}.cache"

now="$(date +%s)"
mtime="$(stat -f %m "$cache_file" 2>/dev/null || printf '0')"

if [ ! -s "$cache_file" ] || [ $((now - mtime)) -ge "$ttl" ]; then
    if command -v kubectl >/dev/null 2>&1; then
        raw="$(kubectl --request-timeout=1s config view --minify -o jsonpath='{.current-context}{"|"}{.contexts[0].context.namespace}' 2>/dev/null || true)"
    else
        raw=""
    fi
    ctx="${raw%%|*}"
    ns="${raw#*|}"

    [ -n "$ctx" ] || ctx="-"
    [ -n "$ns" ] || ns="default"

    printf '%s/%s\n' "$ctx" "$ns" > "$cache_file"
fi

cat "$cache_file"
