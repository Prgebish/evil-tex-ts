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

### Environment asterisk (mte)

```latex
\begin{equation}   -->   \begin{equation*}
```

### Math mode (mtm)

```latex
\(x^2\)   -->   \[x^2\]
```

### Delimiter sizing (mtd)

```latex
(x + y)   -->   \left(x + y\right)
```

### Command asterisk (mtc)

```latex
\section{Title}   -->   \section*{Title}
```

## Surround

### Add environment

```
ysiwe equation
```
Wraps word in `\begin{equation}...\end{equation}`

### Change surrounding

```
csee align
```
Changes environment to `align`

### Delete surrounding

```
dsm
```
Deletes surrounding math delimiters
