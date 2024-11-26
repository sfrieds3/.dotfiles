# rull all pre-commit hooks on all files
pcr-all:
    pre-commit run --all-files

# run stylua on all files
stylua:
    pre-commit run --all-files stylua

# check for merge conflicts on all files
merge-conflicts:
    pre-commit run --all-files check-merge-conflicts

# check for trailing whitespace in all files
whitespace:
    pre-commit run --all-files trailing-whitespace

# setup pre-commit hooks
setup:
    pre-commit install --install-hooks

# run core ansible playbook
core:
    ansible-playbook ./playbooks/core.yaml

# run extras ansible playbook
extras:
    ansible-playbook ./playbooks/extras.yaml

# run mac ansible playbook
mac:
    ansible-playbook ./playbooks/mac.yaml

# run personal ansible playbook
personal:
    ansible-playbook ./playbooks/personal.yaml

# run python ansible playbook
python:
    ansible-playbook ./playbooks/python.yaml

# run update ansible playbook
update:
    ansible-playbook ./playbooks/update.yaml
