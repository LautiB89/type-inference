# Calculadora de inferencia de tipos

Calculadora para verificar resultados y aprender a usar el Algoritmo $\mathcal{I}$ de inferencia de tipos de la materia **Paradigmas de Programación** en FCEyN - UBA.


## Cómo usar la calculadora

1. Escribí el término en el campo de texto. Si es un texto válido (Ver [cómo escribir términos](#Cómo-escribir-términos)), debería poder convertirlo a una expresión de cálculo lambda y mostrarlo.
2. Si necesitás verificar que asociaste bien las aplicaciones podés usar el botón `Mostrar paréntesis implícitos`.
3. Hacé el ejercicio y comprobá tu resultado de cada paso con lo que se muestra en pantalla.

## Cómo escribir términos

> [!WARNING]
> Tener cuidado con las mayúsculas al escribir el término. Es case-sensitive, por lo que no da lo mismo escribir *True* y escribir *true*.

Los términos permitidos están dados por la siguiente gramática (expresada en [BNF](https://es.wikipedia.org/wiki/Notaci%C3%B3n_de_Backus-Naur))

```txt
M ::= x
    | \x. M
    | M M
    | true 
    | false 
    | if M then M else M
    | zero 
    | succ(M) 
    | pred(M) 
    | isZero(M)
```


Las variables empiezan con una minúscula y pueden contener cualquier caracter alfanumérico y `_` dentro.

Además de poder usar `succ`, `pred` y `zero` para escribir `Nat`s, se puede escribir cualquier número y este se convierte a su notación correspondiente. Por ejemplo, escribir `3` da como resultado el término `succ(succ(succ(0)))`.