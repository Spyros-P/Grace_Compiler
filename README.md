```text
_____/\\\\\\\\\\\\________________________________________________________________
 ___/\\\//////////_________________________________________________________________
  __/\\\____________________________________________________________________________
   _\/\\\____/\\\\\\\____/\\/\\\\\\\____/\\\\\\\\\_________/\\\\\\\\______/\\\\\\\\__
    _\/\\\___\/////\\\___\/\\\/////\\\__\////////\\\______/\\\//////_____/\\\/////\\\_
     _\/\\\_______\/\\\___\/\\\___\///_____/\\\\\\\\\\____/\\\___________/\\\\\\\\\\\__
      _\/\\\_______\/\\\___\/\\\___________/\\\/////\\\___\//\\\_________\//\\///////___
       _\//\\\\\\\\\\\\/____\/\\\__________\//\\\\\\\\/\\___\///\\\\\\\\___\//\\\\\\\\\\_
        __\////////////______\///____________\////////\//______\////////_____\//////////__
```
                          
<sub>**Disclaimer**: Any instruction given below is tested and working on Ubuntu 22.04</sub>

### SETUP

```
sudo apt install opam
opam init
opam install ocaml-lsp-server
opam install menhir.20210929
opam install llvm.14.0.6
sudo apt install llvm
sudo apt install clang
eval $(opam env)
```
### USAGE

`./gracec.sh <filename> [-O]`

`./gracec.sh -f [-O]` ( source code : STDIN )

`./gracec.sh -i [-O]` ( source code : STDIN )

`./gracec.sh -i -f [-O]` ( source code : STDIN )

<sub>**Note**: When -f or -i flags are enabled, the code must be provided from STDIN (you can not provide any filename as an argument to compiler)</sub>

### Enable VSCode syntax highlighting

Bash commands:
```bash
sudo apt install npm
npm init -y
sudo npm install -g yo generator-code
yo code
```

If the `yo code` fails, run:
```bash
sudo apt install curl
curl https://raw.githubusercontent.com/creationix/nvm/master/install.sh | bash 
source ~/.profile
nvm install node
```
and then rerun `yo code`

Now complete the `yo code` as we suggest in the picture below:

![Screenshot from 2023-03-21 18-23-44](https://user-images.githubusercontent.com/56654089/226679987-7c045192-9217-4815-95ac-7c8c944826d6.png)


Finally copy the folder `grace_vscode_syntax_highlighting` and paste it inside folder `~/.vscode/extensions`. Additionally you may want to remove/delete any unnecessary files that were created at this process like:
```bash
rm -rf grc package.json
```

<sub>**Hint**: Consider creating a soft link to point to the folder of this repository, in order to update the syntax highlighter easier by pulling any changes from github</sub>
