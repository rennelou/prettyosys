# prettyosys

<!---Esses são exemplos. Veja https://shields.io para outras pessoas ou para personalizar este conjunto de escudos. Você pode querer incluir dependências, status do projeto e informações de licença aqui--->



> *Logic is the art of going wrong with confidence. -* Morris Kline
> 

---

<img src="prettyosys_example.gif" alt="exemplo gif">

The goal of prettyosys is that you just need the hardware description file in VHDL and the property specification file in PSL for verify your hardware. No need of configuration files for every hardware wich you want to verify. No need to descripte of where are your VHDL’s or PSL’s files, the prettyosys searchs recursively for your files in your project. Everything wich you need to do is write you hardware and properties and prettyosys take care of the rest.

---

## Instalation

Prettyosys combine yosys, symbiyosys and ghdl to create a easy enviromment to fomal verify and sinthetize your hardware descriptions in VHDL. For now, this haven’t support for Verilog or SystemVerilog and only support Yices how formal verification tool. You need the ghdl-yosys-plugin to sintetize VHDL in yosys tool too.

Requirements

* yosys
* ghdl
* ghdl-yosys-plugin
* symbiyosys
* yices

The easier way to get the requirements is by **[oss-cad-suite-build](https://github.com/YosysHQ/oss-cad-suite-build)**

### Compile

To compile you need cabal haskell compiler, we recommend to get it by ghcup. When you have a cabal working follow these steps below:

```bash
git clone [git@github.com](mailto:git@github.com):rennelou/prettyosys.git

cd prettyosys

cabal build && cabal install
```

The prettyosys binary will be installed on $HOME/.cabal/bin, we recommend to add this path to yousr$PATH variable on .bashrc file.

---

## Getting Staterd

To bootstrap a prettyosys project, everything you need to do is enter in a folder that you want your project and follow these steps below:

```bash
prettyosys init
```

You will see a prettyosys.toml file that contains the config about the project. A src folder with a counter.vhd file wich is the hardware example bootstraped and a vunits folder with counter.psl wich contains properties to formal verify the counter hardware example.

```bash
prettyosys verify
```

And that’s it! Go ahead and types your own projects

### Ajustes e melhorias

O projeto ainda está em desenvolvimento e as próximas atualizações serão voltadas nas seguintes tarefas:

- [x] Tarefa 1
- [x] Tarefa 2
- [x] Tarefa 3
- [ ] Tarefa 4
- [ ] Tarefa 5

## 💻 Pré-requisitos

Antes de começar, verifique se você atendeu aos seguintes requisitos:
<!---Estes são apenas requisitos de exemplo. Adicionar, duplicar ou remover conforme necessário--->
* Você instalou a versão mais recente de `<linguagem / dependência / requeridos>`
* Você tem uma máquina `<Windows / Linux / Mac>`. Indique qual sistema operacional é compatível / não compatível.
* Você leu `<guia / link / documentação_relacionada_ao_projeto>`.

## 🚀 Instalando <nome_do_projeto>

Para instalar o <nome_do_projeto>, siga estas etapas:

Linux e macOS:
```
<comando_de_instalação>
```

Windows:
```
<comando_de_instalação>
```

## ☕ Usando <nome_do_projeto>

Para usar <nome_do_projeto>, siga estas etapas:

```
<exemplo_de_uso>
```

Adicione comandos de execução e exemplos que você acha que os usuários acharão úteis. Fornece uma referência de opções para pontos de bônus!

## 📫 Contribuindo para <nome_do_projeto>
<!---Se o seu README for longo ou se você tiver algum processo ou etapas específicas que deseja que os contribuidores sigam, considere a criação de um arquivo CONTRIBUTING.md separado--->
Para contribuir com <nome_do_projeto>, siga estas etapas:

1. Bifurque este repositório.
2. Crie um branch: `git checkout -b <nome_branch>`.
3. Faça suas alterações e confirme-as: `git commit -m '<mensagem_commit>'`
4. Envie para o branch original: `git push origin <nome_do_projeto> / <local>`
5. Crie a solicitação de pull.

Como alternativa, consulte a documentação do GitHub em [como criar uma solicitação pull](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/creating-a-pull-request).

## 🤝 Colaboradores

Agradecemos às seguintes pessoas que contribuíram para este projeto:

<table>
  <tr>
    <td align="center">
      <a href="#">
        <img src="https://avatars3.githubusercontent.com/u/31936044" width="100px;" alt="Foto do Iuri Silva no GitHub"/><br>
        <sub>
          <b>Iuri Silva</b>
        </sub>
      </a>
    </td>
    <td align="center">
      <a href="#">
        <img src="https://s2.glbimg.com/FUcw2usZfSTL6yCCGj3L3v3SpJ8=/smart/e.glbimg.com/og/ed/f/original/2019/04/25/zuckerberg_podcast.jpg" width="100px;" alt="Foto do Mark Zuckerberg"/><br>
        <sub>
          <b>Mark Zuckerberg</b>
        </sub>
      </a>
    </td>
    <td align="center">
      <a href="#">
        <img src="https://miro.medium.com/max/360/0*1SkS3mSorArvY9kS.jpg" width="100px;" alt="Foto do Steve Jobs"/><br>
        <sub>
          <b>Steve Jobs</b>
        </sub>
      </a>
    </td>
  </tr>
</table>


## 😄 Seja um dos contribuidores<br>

Quer fazer parte desse projeto? Clique [AQUI](CONTRIBUTING.md) e leia como contribuir.

## 📝 Licença

Esse projeto está sob licença. Veja o arquivo [LICENÇA](LICENSE.md) para mais detalhes.

[⬆ Voltar ao topo](#nome-do-projeto)<br>
