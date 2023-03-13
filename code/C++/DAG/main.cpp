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

using namespace std;

int main(int argc, char* argv[]) {

    string path = "/home/maribel/Escritorio/5º DGIIM/TFG/Analysis-of-processes/code/C++/ImportDisco/data/";

    if (argc != 3){
        cerr << "Número de argumentos erróneo." << endl << endl;
        cerr << "El programa se ha de ejecutar de la siguiente manera: ./pruebaDAG path filename" << endl;
        return 1;
    }

    if (strcmp(argv[1],"1") == 0)
        path = path + "DISCO_complete/";
    else if (strcmp(argv[1],"2") == 0)
        path = path + "DISCO_complete_segmentated/";
    else if (strcmp(argv[1],"3") == 0)
        path = path + "DISCO_compound/";
    else{
        cerr << "El directorio escogido es erróneo." << endl << endl;
        cerr << "Indique 1 para seleccionar el directorio DISCO_complete o" << endl;
        cerr << "2 para seleccionar el directorio DISCO_compound." << endl;
        return 1; 
    }

    string filename = argv[2];
    DAG dag(path + filename);

    if (!dag.is_correct()){
        return 1;
    }

    // cout << dag << endl;

    /*cout << "Caminos posibles:" << endl;
    for (const auto& element : dag.get_names()) {
        for (const auto& ele : element)
            cout << ele << " ";
        cout << endl;
    }

    cout << endl << "Coeficiente: " << dag.get_coefficient() << endl;*/

    cout << dag.get_coefficient();

    return 0;
}
