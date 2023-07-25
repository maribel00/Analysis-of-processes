# Asistente para el descubrimiento de procesos de aprendizaje ocultos durante la realización de prácticas de laboratorio

## Descripción

Proporcione una breve descripción que explique el qué, el por qué y el cómo de su proyecto. Utilice las siguientes preguntas como guía:

- ¿Cuál fue su motivación?
- ¿Por qué desarrolló este proyecto? (Nota: La respuesta no es “Porque fue una asignación de tarea”.)
- ¿Qué problema resuelve?
- ¿Qué aprendió?

## Índice

- [Contenido](#contenido)
- [Manual](#manual)
- [Créditos](#créditos)
- [Licencia](#licencia)

## Contenido

Como podemos ver, el repositorio está divido en dos grandes partes: la documentación (carpeta `documentation`) y el código asociado a la misma (carpeta `code`).

En la carpeta `Documentation`, se destaca:
* El directorio `add_ons`
* El directorio `chapters`, que contiene los archivos `.tex` de cada uno de los capítulos que conforman la memoria.
* El directorio `figures`, donde se guardan todas las figuras que se incluirán en la documentación del proyecto.
* El arhivo `main.tex`, que se encarga de crear la estructura de la memoria y de incluir todos los archivos de la carpeta `add_ons` y de la carpeta `chapters` de los que se compone.
* El archivo `main.pdf` que nos más que el resultado de la compilación del archivo `main.tex`.

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

## Manual

Proporcione instrucciones y ejemplos de uso. Incluya capturas de pantalla según sea necesario.

Para agregar una captura de pantalla, cree una carpeta `assets/images` en su repositorio y cargue la captura de pantalla en ella. Luego, con la ruta relativa, agréguela a su README utilizando la siguiente sintaxis:

    ```md
    ![alt text](assets/images/screenshot.png)
    ```

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