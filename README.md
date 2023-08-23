# Asistente para el descubrimiento de procesos de aprendizaje ocultos durante la realización de prácticas de laboratorio

## 🗂 Índice

- [Descripción del proyecto](#📝-descripción-del-proyecto)
- [Objetivos del proyecto](#✔-objetivos)
- [Contenido del repositorio](#💻-contenido-del-repositorio)
- [Manual de uso](#📖-manual-de-uso)
- [Créditos](#✒-créditos)
- [Licencia](#⚖-licencia)

## 📝 Descripción del proyecto

El estudio del proceso de aprendizaje que realizan los alumnos cuando se les platean una serie de tareas es fundamental puesto que éste podría facilitar la asimilación de nuevos conocimientos y de hacer accesible aquello que se enseña al estudiantado.

El objetivo principal de este trabajo es, precisamente, identificar los patrones de comportamiento de aquellos estudiantes en riesgo de obtener un rendimiento más bajo de lo esperado con el fin de permitir una temprana intervención del personal docente, evitando así el fracaso de los mismos en la adquisión de nuevos conocimientos.

Así pues, en este trabajo fin de grado se usarán datos obtenidos en un laboratorio virtual para, a través de técnicas de Minería de Procesos, extraer una serie de grafos, representados mediante matrices, que reflejan el comportamiento de los alumnos en la plataforma tal y como se demostrará en este estudio.

Una parte importante del mismo ha sido el desarrollo de una herramienta de Minería de Procesos de creación propia, *Graph Miner*, que se encarga de traducir los registros del labotorio virtual en las matrices que representan el comportamiento de los alumnos. Como veremos, ésta suplirá los inconvenientes del programa de Minería de Procesos ya existente, *Disco*.

Adicionalmente, se usarán técnicas de aprendizaje automático supervisado no sólo para preveer grupos en riesgo sino también para predecir en qué intervalo de notas se encuentra la calificación de los diferentes grupos de alumnos con evidencias estadísticas. De hecho, se pueden realizar tales predicciones con una alta fiabilidad en estadios tempranos del desarrollo de la práctica.

Por último, destacar que, para realizar las clasificaciones descritas en el párrafo anterior, se han utilizado tanto medidas clásicas del rendimiento de los alumnos como medidas de complejidad basadas únicamente en la topología del grafo representado a través de la matriz característica de cada grupo, demostrando éstas últimas ser igual de útiles que las primeras.

## ✔ Objetivos

El principal objetivo de este proyecto es identificar patrones de comportamiento indicativos de la evolución de los alumnos y del progreso de su aprendizaje, detectando, en las fases más tempranas posibles, comportamientos que pudiesen ser anómalos o que pudiesen indicar problemas de aprendizaje. Es decir, se pretende relevar, mediante la utilización de técnicas de minería de procesos, las posibles estrategias de los alumnos para cumplir los distintos objetivos de la asignatura así como desvelar su forma de trabajo habitual.

## 💻 Contenido del repositorio

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

* El directorio `Preliminary analyses` recoge los estudios realizados justo al comienzo del proyecto. Contiene los siguientes archivos:
    - `TestHomogeneidad.R` se encarga de la realización del un test *ANOVA* de dos factores con el objetivo de garantizar que el dataset que tenemos es homogéneo.
    - `FiltrarRegistros.R` se encarga de filtrar y limpiar el dataset original `DBA1520.csv` y de almacenar el nuevo dataset en el archivo `cleandataset.csv`.
    - `Segmentar.R` se encarga de segmentar el dataset `cleandataset.csv` en distintos datasets en función de las calificaciones obtenidas por cada uno de los grupos de prácticas. Las calificaciones de los grupos pueden encontrarse en `DBA1520GRADED.csv`.
    - `Estudio.R` se encarga de realizar un estudio estadístico de los datasets resultantes de la segmentación realiza como el script `Segmentar.R`.
* El directorio

#### datasets

* El dataset de partida, `DBA1520.csv`, que no contiene los registros de todos los años (sólo contiene datos de los cursos académicos 2015-2016, 2016-2017, 2017-2018 y 2019-2020).
* El dataset `cleandataset.csv` no es más que el dataset `DBA1520.csv` limpio (sin datos erróneos). Se genera a través del script `FiltrarRegistros.R`.
* El dataset `DBA1520GRADED.csv`, que contiene una fila por cada uno de los grupos de prácticas en la que se indica su nombre, el número de integrantes, el año al que pertenecen y la calificación obtenida por los mismos.
* Los datasets `datasetS.csv`, `datasetA.csv`, `datasetN.csv`, `datasetSOB.csv`, `datasetMH.csv` corresponden a la segmentación del dataset `cleandataset.csv` por notas. Se generan mediante el script `Segmentar.R`.

## 📖 Manual de uso

Para obtener los grafos correspondientes al proceso de aprendizaje de cada uno de los grupos se deberá tener instalado Rscript. A continuación, se deberá la siguiente instrucción en el directorio `/code/R`:

`Rscript generate.R`

No obstante, el proceso de generación de todos los grafos es costoso. Así pues, ya se proporcionan los mismos en las carpetas `Graphs`, `GraphsSummary`, `GraphsProblems`, `GraphsStates` y `GraphsStates_wc`.

Para reproducir los experimentos cuyos resultados se muestran en los *Capítulos 13* y *14* de la memoria se deberá ejecutar en el directorio `/code/R` las siguientes líneas:

`Rscript main.R`

## ✒ Créditos

Este trabajo fin de grado ha sido tutorizado por Luis Castillo Vidal, Catedrático de Universidad del Departamento de Ciencias de la Computación e Inteligencia Artificial de la Universidad de Granada.

## ⚖ Licencia

El contenido de este repositorio está sujeto a la licencia [MIT](https://github.com/maribel00/Analysis-of-processes/blob/f2ce972be1725cbb0f038a191eeca4421cffe0ca/LICENSE).
