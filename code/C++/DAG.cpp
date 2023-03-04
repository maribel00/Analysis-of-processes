#include <fstream>
#include <sstream>
#include "DAG.h"

using namespace std;

DAG::DAG(string filename) {
    ifstream file(filename);

    // Omitir la primera línea del archivo
    string line;
    getline(file, line);

    bool found_frequency = false;
    bool found_duration = false;

    while (getline(file, line)) {
        if (!found_frequency && line.find("Total Frequency") != string::npos) {
            getline(file, line);
            found_frequency = true;
            continue;
        } else if (found_frequency && line.find("Total Duration ms") != string::npos) {
            getline(file, line);
            found_duration = true;
            continue;
        }

        if (found_frequency && !found_duration) {
            vector<int> row;
            stringstream ss(line);
            string cell;
            getline(ss, cell, ','); // Ignorar el primer elemento de la fila
            this->header.push_back(cell);
            while (getline(ss, cell, ',')) {
                row.push_back(stoi(cell));
            }

            this->frequency.push_back(row);
        } else if (found_duration) {
            vector<int> row;
            stringstream ss(line);
            string cell;
            getline(ss, cell, ','); // Ignorar el primer elemento de la fila
            while (getline(ss, cell, ',')) {
                row.push_back(stoi(cell));
            }

            this->duration.push_back(row);
        }
    }

    this->frequency.pop_back();

    this->size = frequency[0].size(); // Obtenemos el número de columnas
}

vector<string> DAG::split(string id) {
    int posInit = 0;
    int posFound = 0;
    string splitted;
    vector<string> results;
    
    posFound = id.find('M', posInit);
    posInit = posFound + 1;

    posFound = id.find('-', posInit);
    splitted = id.substr(posInit, posFound - posInit);
    posInit = posFound + 1;
    results.push_back(splitted);

    int posEnd = id.length();
    splitted = id.substr(posInit, posEnd);
    results.push_back(splitted);
    
    return results;
}

int DAG::num_problems(vector<string> path){
    int num_problems = 0;
    vector<string> old;

    if (path.size() >= 2){ // TODO: Eliminar?
        old = split(path.front());
        num_problems++;
        for (int i = 2; i < path.size(); ++i){
            vector<string> actual = split(path[i]);
            if (actual.front().compare(old.front()) != 0)
                num_problems++;
            old = split(path[i]);
        }
    }

    return num_problems;
}

int DAG::find_paths(int row){
    if (row == (size-1)) // Hemos llegado a END
        return 0;
    else if (row == (size-2)){ // Encontrar los nodos después de START
        for (int col = 0; col < size; ++col){
            if (frequency[row].at(col) > 0){
                vector<int> path;
                path.push_back(col);
                paths.push_back(path);
                string name = header[col];
                vector<string> aux;
                aux.push_back(name);
                names.push_back(aux);
                find_paths(col);
            }
        }
    }
    else {
        vector<int> index;
        int i = 0;
        for (const auto& list : paths) {
            if (list.back() == row)
                index.push_back(i);
            ++i;
        }

        vector<int> columns;
        for (int col = 0; col < size; ++col){
            if (frequency[row].at(col) > 0){
                /*string name = header[col];
                for (const auto& ele : index){
                    paths[ele].push_back(col);
                    names[ele].push_back(name);
                }
                find_paths(col);*/
                columns.push_back(col);
            }
        }
        if (columns.size() == 1){
            string name = header[col];
            for (const auto& ele : index){
                    paths[ele].push_back(col);
                    names[ele].push_back(name);
            }
            find_paths(col);
        }
        else {
            for (const auto& column : columns){
                for (const auto& ele : index){
                    vector<int> new_path;
                    vector<string> new_sequence;
                    for (const auto& integer : paths[ele])
                        new_path.push_back(integer);
                    for (const auto& name : names[ele])
                        new_sequence.push_back(name);

                    paths.push_back(new_path);
                    names.push_back(new_sequence);
                }
            }
        }
    }
}

float DAG::calculate_coefficient(){
    return 0.0f;
}

ostream& operator<<(ostream& ostr, const DAG& dag) {
    // Imprime los datos de ambas matrices
    ostr << endl;

    ostr << "FREQUENCY:" << endl;
    for (const auto& row : dag.frequency) {
        for (const auto& cell : row) {
            ostr << cell << " ";
        }
        ostr << endl;
    }

    ostr << endl;

    ostr << "DURATION:" << endl;
    for (const auto& row : dag.duration) {
        for (const auto& cell : row) {
            ostr << cell << " ";
        }
        ostr << endl;
    }

    ostr << endl;

    // Imprime la cabecera
    ostr << "HEADER:" << endl;
    for (const auto& cell : dag.header) {
        ostr << cell << " ";
    }
    ostr << endl;

    return ostr;
}
