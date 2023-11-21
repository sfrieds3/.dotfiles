function fish_set_virtual_env --description "issue with path persisting in fish 3.0, this is a roundabout way to ensure virtualenv is always at head of PATH"

    if set -q VIRTUAL_ENV
        set VENV_BIN $VIRTUAL_ENV/bin

        # remove existing venv bin from path
        if set -l index (contains -i $VENV_BIN $PATH)
            set -e PATH[$index]
        end

        # and add to beginning of path
        set --export PATH $VENV_BIN $PATH
    end
end
