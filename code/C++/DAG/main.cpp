/**
 * @file main.cpp
 * @author María Isabel Ruiz Martínez (you@domain.com)
 * @brief 
 * @version 0.1
 * @date 2023-03-06
 * 
 * @copyright Copyright (c) 2023
 * 
 */

#include "DAG.h"
#include <iostream>
#include <cstring>
#include <fstream>

using namespace std;

int main(int argc, char* argv[]) {

    string path = "../../Python/matrices/";
    
    if (argc != 2){
        cerr << "Número de argumentos erróneo." << endl << endl;
        cerr << "El programa se ha de ejecutar de la siguiente manera: ./pruebaDAG filename" << endl;
        return 1;
    }

    string filename = argv[1];
    DAG dag(path + filename);

    if (!dag.is_correct()){
        return 1;
    }

    size_t last_bar = filename.find_last_of("/"); // Encontrar la última barra
    string name = filename.substr(last_bar + 1); // Extraer subcadena a partir de la última barra
    size_t extension = name.find_last_of("."); // Encontrar la última extensión
    string name_without_extension = name.substr(0, extension); // Extraer subcadena sin extensión

    cout << name_without_extension << " " << dag.get_coefficient();

    return 0;
}
