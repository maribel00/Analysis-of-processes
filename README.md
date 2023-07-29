# Asistente para el descubrimiento de procesos de aprendizaje ocultos durante la realización de prácticas de laboratorio

## Descripción



## Índice

- [Contenido](#contenido)
- [Manual](#manual)
- [Créditos](#créditos)
- [Licencia](#licencia)

## Contenido

Este directorio presenta la estructura que se muestra a continuación:

```
📦Analysis-of-processes
 ┣ 📂code
 ┣ 📂documentation
 ┣ 📜.gitignore
 ┣ 📜LICENSE
 ┗ 📜README.md
```

Como podemos ver, el repositorio está divido en dos grandes partes: la documentación (carpeta `documentation`) y el código asociado a la misma (carpeta `code`).

### Documentación

La carpeta `Documentation` presenta la siguiente estructura interna:

```
📂documentation
 ┗ 📂classicthesis-tex-es
 ┃ ┣ 📂_minted-main
 ┃ ┣ 📂add_ons
 ┃ ┃ ┣ 📜agradecimientos.tex
 ┃ ┃ ┣ 📜autorizacion.tex
 ┃ ┃ ┣ 📜originalidad.tex
 ┃ ┃ ┣ 📜resumen.tex
 ┃ ┃ ┣ 📜summary.tex
 ┃ ┃ ┗ 📜tutor.tex
 ┃ ┣ 📂chapters
 ┃ ┣ 📂figures
 ┃ ┣ 📜.gitignore
 ┃ ┣ 📜apa-good.bst
 ┃ ┣ 📜bussproofs.sty
 ┃ ┣ 📜classicthesis-config.tex
 ┃ ┣ 📜classicthesis.sty
 ┃ ┣ 📜macros.tex
 ┃ ┣ 📜main.pdf
 ┃ ┣ 📜main.tex
 ┃ ┣ 📜makefile
 ┃ ┣ 📜research.bib
 ┃ ┣ 📜slashbox.sty
 ┃ ┣ 📜slashbox.tex
 ┃ ┣ 📜texput.log
 ┃ ┣ 📜titlepage.tex
 ┃ ┗ 📜ugrA4.pdf
 ```

En ella, se destaca:
* El directorio `add_ons`, donde se encuentra algunos archivos auxiliares que contienen la declaración de originalidad (`originalidad.tex`), la autorización para la ubicación de la memoria en la biblioteca (`autorizacion.tex`), la autorización para su defensa (`tutor.tex`), los resuménes del proyecto tanto en inglés como en español (`resumen.tex` y `summary.tex`) y los agradecimientos (`agradecimientos.tex`).
* El directorio `chapters`, que contiene los archivos `.tex` de cada uno de los capítulos que conforman la memoria.
* El directorio `figures`, donde se guardan todas las figuras que se incluirán en la documentación del proyecto.
* El arhivo `main.tex`, que se encarga de crear la estructura de la memoria y de incluir todos los archivos de la carpeta `add_ons` y de la carpeta `chapters` de los que se compone.
* El archivo `main.pdf` que nos más que el resultado de la compilación del archivo `main.tex`.

### Código

Dentro de la carpeta `Code`, encontramos:

```
📂code
 ┣ 📂C++
 ┣ 📂Python
 ┣ 📂R
 ┗ 📂datasets
 ┃ ┣ 📜.RData
 ┃ ┣ 📜.Rhistory
 ┃ ┣ 📜DBA1520.csv
 ┃ ┣ 📜DBA1520GRADED.csv
 ┃ ┣ 📜cleandataset.csv
 ┃ ┣ 📜datasetA.csv
 ┃ ┣ 📜datasetMH.csv
 ┃ ┣ 📜datasetN.csv
 ┃ ┣ 📜datasetS.csv
 ┃ ┗ 📜datasetSOB.csv
```

* Una carpeta que contiene los datasets que se han empleado en la realización de los experimentos.
* La carpeta `C++` que contiene código escrito en C++.
* La carpeta `Python` que contiene una serie de notebooks con primeras inspecciones de los datasets y una primera versión de la aplicación final de este proyecto.
* La carpeta `R`, de gran relevancia al contener tanto la aplicación final de este trabajo como todos los análisis estadísticos presentados en la memoria del proyecto. 

#### C++

#### Python

#### R

#### datasets

* El dataset
* Los datasets `datasetS.csv`, `datasetA.csv`, `datasetN.csv`, `datasetSOB.csv`, `datasetMH.csv` corresponden a la segmentación del dataset por notas.

## Manual

Para obtener los grafos correspondientes al proceso de aprendizaje de cada uno de los grupos se deberá tener instalado Rscript. A continuación, se deberá la siguiente instrucción en el directorio `/code/R`:

`Rscript generate.R`

No obstante, el proceso de generación de todos los grafos es costoso. Así pues, ya se proporcionan los mismos en las carpetas `Graphs`, `GraphsSummary`, `GraphsProblems`, `GraphsStates` y `GraphsStates_wc`.

Para reproducir los experimentos cuyos resultados se muestran en los *Capítulos 12* y *13* de la memoria se deberá ejecutar en el directorio `/code/R` las siguientes líneas:

`Rscript`

`Rscript`

## Crédito

Enumere sus colaboradores, si los hubiera, con enlaces a sus perfiles de GitHub.

Si utilizó activos de terceros que requieren autoría, enumere los creadores con enlaces a su presencia web principal en esta sección.

Si siguió tutoriales, también incluya enlaces a ellos aquí.

## Licencia

La última sección de un archivo README de alta calidad es la licencia. Esto permite que otros desarrolladores sepan lo que pueden y no pueden hacer con su proyecto. Si necesita ayuda para elegir una licencia, consulte [https://choosealicense.com/](https://choosealicense.com/).

## Funciones

Si su proyecto tiene muchas funciones, enumérelas aquí.

## Pruebas

Vaya un paso más allá y escriba pruebas para su aplicación. Luego, proporcione ejemplos sobre cómo ejecutarlas aquí.