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
    else if (strcmp(argv[1],"4") == 0)
        path = path + "DISCO_segmentated_groups/";
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

    ofstream file("test.txt");
    vector<string> labels = dag.get_header();

    for (const auto& element : labels) {
        file << element << " ";
    }
    file << std::endl;

    vector<vector<int>> frequency = dag.get_frequency();

    for (const auto& innerVector : frequency) {
        for (const auto& element : innerVector) {
            file << element << " ";
        }
        file << std::endl;
    }

    file.close();

    cout << dag.get_coefficient() << " " << dag.get_entropy();

    return 0;
}
