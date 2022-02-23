# Futhark Language Server

Working in progress

## Development

Current version depends on the unmerged change in [diku-dk/futhark#1603](https://github.com/diku-dk/futhark/pull/1603). To build successfully, you need to clone `futhark` into same level as `futhark-language-server`, and checkout the corresponding pull request, so the file structure looks like:

```shell
├── futhark-language-server
│   ├── cabal.project
│   ...
├── futhark
│   ├── futhark.cabal
...
```

## Usage

To use the language server in vscode, checkout [vscode-futhark](https://github.com/haoranpb/vscode-futhark).
