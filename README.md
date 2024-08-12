# dotfiles

Pre-reqs:

1) install Homebrew
2) `brew install ansible`


To install:

```sh
ansible-playbook playbooks/system.yaml
ansible-playbook playbooks/mac.yaml
```

For an individual task:

```sh
ansible localhost -m import_role -a name=nvim
```
