#include "DAG.h"
#include <iostream>
#include <string>

using namespace std;

int main(int argc, char* argv[]) {

    string path = "/home/maribel/Escritorio/5º DGIIM/TFG/Analysis-of-processes/code/C++/ImportDisco/data/DISCO_compound/";

    if (argc != 2){
        cerr << "Número de argumentos érroneo." << endl << endl;
        cerr << "El programa se ha de ejecutar de la siguiente manera: ./pruebaDAG filename" << endl;
        return 1;
    }

    string filename = argv[1];
    DAG dag(path + filename);

    if (!dag.get_file()){
        return 1;
    }

    cout << dag << endl;

    cout << "Caminos posibles:" << endl;
    for (const auto& element : dag.get_names()) {
        for (const auto& ele : element)
            cout << ele << " ";
        cout << endl;
    }

    cout << endl << "Coeficiente: " << dag.get_coefficient() << endl;

    return 0;
}
