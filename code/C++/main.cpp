#include "DAG.h"
#include <iostream>
#include <string>

using namespace std;

int main() {

    string path = "/home/maribel/Escritorio/5º DGIIM/TFG/Analysis-of-processes/code/C++/ImportDisco/data/DISCO_compound/";
    string filename = "Year1920WorstGrades.csv";
    DAG dag(path + filename);

    cout << dag << endl;

    // Prueba función split
    // vector<string> results = dag.split("PROBLEM1-20");

    /* for (const auto& element : results) {
        cout << element << endl;
    }*/

    // Prueba función num_problems
    /* vector<string> path_2;
    string s = "PROBLEM1-20";
    path_2.push_back(s);
    s = "PROBLEM1-40";
    path_2.push_back(s);
    s = "PROBLEM2-40";
    path_2.push_back(s);
    s = "PROBLEM9-20";
    path_2.push_back(s);
    s = "PROBLEM9-40";
    path_2.push_back(s);
    s = "PROBLEM6-80";
    path_2.push_back(s);
    int num_problems = dag.num_problems(path_2);

    cout << "NUM_PROBLEMS: " << num_problems << endl;*/

    dag.find_paths(dag.get_size()-2);

    for (const auto& element : dag.names) {
        for (const auto& ele : element)
            cout << ele << " ";
        cout << endl;
    }

    return 0;
}
