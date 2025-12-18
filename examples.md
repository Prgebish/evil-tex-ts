# evil-tex-bora: Examples

This file contains usage examples for evil-tex-bora.

## Text Objects

### Environment (ie/ae)

```latex
\begin{equation}
  x^2 + y^2 = z^2|
\end{equation}
```
- `vie` - select inner environment (the formula)
- `vae` - select entire environment including `\begin` and `\end`
- `cie` - change inner environment
- `dae` - delete entire environment

### Command (ic/ac)

```latex
\textbf{hello| world}
```
- `vic` - select `hello world`
- `vac` - select `\textbf{hello world}`
- `cic` - change command argument
- `dac` - delete entire command

### Math (im/am)

```latex
\(x^2 + y^2|\ = z^2\)
```
- `vim` - select `x^2 + y^2 = z^2`
- `vam` - select `\(x^2 + y^2 = z^2\)`

### Delimiter (id/ad)

```latex
\left(a + b|\right)
```
- `vid` - select `a + b`
- `vad` - select `\left(a + b\right)`

## Toggles

All toggle commands use the `mt` prefix by default (mnemonic: "magnificent toggle").

**Note:** The `mt` prefix overrides the `m` (set-marker) command for the `t` character.
You have 25 other marks available. To change this behavior:

```elisp
;; Disable mt* bindings (preserve original m behavior)
(setq evil-tex-bora-toggle-override-m nil)

;; Use ts* instead of mt* (e.g., tse, tsm, tsd, tsc)
(setq evil-tex-bora-toggle-override-t t)

;; Or use both mt* and ts*
(setq evil-tex-bora-toggle-override-m t)
(setq evil-tex-bora-toggle-override-t t)
```

### Environment asterisk (mte)

Toggle the asterisk on LaTeX environments. Useful for switching between numbered and unnumbered equations.

```latex
% Before: numbered equation
\begin{equation}
  E = mc^2
\end{equation}

% After mte: unnumbered equation
\begin{equation*}
  E = mc^2
\end{equation*}
```

Works with any environment:
```latex
\begin{align}    <-->   \begin{align*}
\begin{figure}   <-->   \begin{figure*}
\begin{table}    <-->   \begin{table*}
```

### Math mode (mtm)

Toggle between inline and display math modes.

```latex
% Before: inline math (either syntax)
The formula $x^2 + y^2 = z^2$ is famous.
The formula \(x^2 + y^2 = z^2\) is famous.

% After mtm: display math
The formula \[x^2 + y^2 = z^2\] is famous.
```

Supports all common formats:
```latex
$...$     ->  \[...\]    (inline to display)
\(...\)   ->  \[...\]    (inline to display)
\[...\]   ->  $...$      (display to inline, default)
```

The inline format used when converting from display is configurable:
```elisp
;; Default: use $...$ (shorter)
(setq evil-tex-bora-preferred-inline-math 'dollar)

;; Alternative: use \(...\) (LaTeX2e style)
(setq evil-tex-bora-preferred-inline-math 'paren)
```

### Delimiter sizing (mtd)

Toggle automatic delimiter sizing with `\left`/`\right`.

```latex
% Before: plain parentheses
\(\frac{a}{b} + (x + y)\)

% After mtd on the parentheses: auto-sized
\(\frac{a}{b} + \left(x + y\right)\)
```

Works with different delimiter types:
```latex
(x + y)     <-->   \left(x + y\right)
[a, b]      <-->   \left[a, b\right]
\{1, 2\}    <-->   \left\{1, 2\right\}
```

Also removes `\bigl`/`\bigr` and similar sizing commands (one-way):
```latex
\bigl(x + y\bigr)    -->   (x + y)
\Bigl[a, b\Bigr]     -->   [a, b]
\biggl(f(x)\biggr)   -->   (f(x))
```

### Command asterisk (mtc)

Toggle the asterisk on LaTeX commands. Useful for switching between numbered and unnumbered sections.

```latex
% Before: numbered section
\section{Introduction}

% After mtc: unnumbered section
\section*{Introduction}
```

Works with various sectioning commands:
```latex
\chapter{Title}      <-->   \chapter*{Title}
\section{Title}      <-->   \section*{Title}
\subsection{Title}   <-->   \subsection*{Title}
\paragraph{Title}    <-->   \paragraph*{Title}
```

Also works with other commands that support asterisk variants:
```latex
\includegraphics{img}   <-->   \includegraphics*{img}
\newcommand{\foo}{}     <-->   \newcommand*{\foo}{}
```

## Surround

Requires [evil-surround](https://github.com/emacs-evil/evil-surround) package.

### Surround Keys

| Key | Surrounds with | Example |
|-----|---------------|---------|
| `m` | `\(...\)` | `ysiwm` → `\(word\)` |
| `M` | `\[...\]` | `ysiwM` → `\[word\]` |
| `c` | Command (prompts) | `ysiwc textbf` → `\textbf{word}` |
| `e` | Environment (keymap) | `ysiwe e` → `\begin{equation}word\end{equation}` |
| `d` | Delimiter (keymap) | `ysiwd p` → `\left(word\right)` |
| `;` | CDLaTeX accent (keymap) | `ysiw; b` → `\textbf{word}` |
| `q` | Single quote | `ysiwq` → `` `word' `` |
| `Q` | Double quote | `ysiwQ` → ``` ``word'' ``` |
| `^` | Superscript | `ysiw^` → `^{word}` |
| `_` | Subscript | `ysiw_` → `_{word}` |
| `T` | Table cell | `ysiwT` → `&word&` |

### Environment Keymap (e)

When using `e` for environment surround, a keymap is shown:

| Key | Environment |
|-----|-------------|
| `x` | Prompt for name |
| `e` | equation |
| `E` | equation* |
| `a` | align |
| `A` | align* |
| `f` | figure |
| `i` | itemize |
| `I` | enumerate |
| `c` | cases |
| `g` | gather |
| `m` | multline |
| `z` | tikzpicture |
| `ta` | axiom |
| `tt` | theorem |
| `tl` | lemma |
| `tp` | proof |
| `td` | definition |

### Delimiter Keymap (d)

| Key | Delimiter |
|-----|-----------|
| `P` | `()` |
| `p` | `\left(\right)` |
| `S` | `[]` |
| `s` | `\left[\right]` |
| `C` | `\{\}` |
| `c` | `\left\{\right\}` |
| `R` | `\langle \rangle` |
| `r` | `\left\langle \right\rangle` |
| `V` | `\lvert \rvert` |
| `v` | `\left\lvert\right\rvert` |
| `N` | `\lVert \rVert` |
| `n` | `\left\lVert\right\rVert` |

### CDLaTeX Accents Keymap (;)

Context-aware: uses `\mathbf` in math, `\textbf` outside.

| Key | Accent | Key | Accent |
|-----|--------|-----|--------|
| `.` | dot | `~` | tilde |
| `^` | hat | `-` | bar |
| `T` | overline | `_` | underline |
| `{` | overbrace | `}` | underbrace |
| `>` | vec | `q` | sqrt |
| `r` | rm | `i` | it |
| `b` | bf | `e` | emph |
| `y` | tt | `f` | sf |
| `c` | mathcal | | |

### Examples

**Add environment:**
```
ysiwe equation
```
Wraps word in `\begin{equation}...\end{equation}`

**Change surrounding:**
```
csee align
```
Changes environment to `align`

**Delete surrounding:**
```
dsm
```
Deletes surrounding math delimiters

**Add inline math:**
```
ysiwm
```
Wraps word in `\(...\)`

**Add command:**
```
ysiwc textbf<CR>
```
Wraps word in `\textbf{...}`

**Add delimiter with sizing:**
```
ysiwd p
```
Wraps in `\left(...\right)`

**Add accent:**
```
ysiw; b
```
Wraps in `\textbf{...}` (or `\mathbf{...}` in math)
