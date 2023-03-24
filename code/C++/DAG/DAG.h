/**
 * @file DAG.h
 * @author María Isabel Ruiz Martínez (you@domain.com)
 * @brief Definition of the class DAG
 * @version 0.1
 * @date 2023-03-06
 * 
 * @copyright Copyright (c) 2023
 * 
 */

#ifndef DAG_H
#define DAG_H

#include <string>
#include <vector>
#include <iostream>

using namespace std;

/**
 * @class DAG 
 *
 * @brief Class representing a Directed Acyclic Graph
 * 
 * This class defined a serie of functions to explore
 * this type of graphs.
 */
class DAG {
    private:
        vector<vector<int>> frequency; /**< matrix representing frequency of the nodes */
        vector<vector<unsigned int>> duration; /**< matrix representing the time spent in each of the nodes */
        bool file_correct; /**< if the file have been opened correctly */
        int size; /**< size of the matrices */
        vector<string> header; /**< header of the matrices */
        vector<vector<int>> paths; /**< all the possible paths */
        vector<vector<string>> names; /**< all the paths with names */

        /**
         * @brief Split the identifier of a problem
         * 
         * The identifier of a problem has the form 
         * PROBLEM1-20 where 1 is the problem number
         * and 20 is the percentage completed.
         * 
         * @param id 
         * @return vector<string> 
         */
        vector<string> split(string id);

        /**
         * @brief Return the number of different
         * problems appearing in a given path
         * 
         * @param path 
         * @return int 
         */
        int num_problems(vector<string> path);

        /**
         * @brief Find the indeces of the paths
         * whose last element is row
         * 
         * @param row 
         * @return vector<int> 
         */
        vector<int> find_index(int row);

        /**
         * @brief Find the non-zero columns of the
         * frequency matrix of a given row
         * 
         * @param row 
         * @return vector<int> 
         */
        vector<int> find_columns(int row);

        /**
         * @brief Remove the duplicated paths both
         * in the paths and names variables
         * 
         */
        void remove_duplicates();

        /**
         * @brief Calculate all the different paths
         * of a DAG removing cycles starting from the
         * node START and finishing in the node END
         * 
         * @param row 
         * @return int 
         */
        int find_paths(int row);

        /**
         * @brief Indicate whether a vector has duplicates
         * or not
         * 
         * @param v 
         * @return true 
         * @return false 
         */
        bool duplicates(vector<int> v);

        /**
         * @brief Implements the floyd warshall algorithm
         * 
         * @return int 
         */
        int floyd_warshall();
    public:
        /**
         * @brief Construct a new DAG object
         * 
         * @param filename 
         */
        DAG(string filename);

        /**
         * @brief Get the frequency object
         * 
         * @return vector<vector<int>> 
         */
        vector<vector<int>> get_frequency(){ return frequency; };

        /**
         * @brief Get the duration object
         * 
         * @return vector<vector<unsigned>> 
         */
        vector<vector<unsigned>> get_duration(){ return duration; };

        /**
         * @brief Indicate whether the given file has been
         * opened correctly or not
         * 
         * @return true 
         * @return false 
         */
        bool is_correct(){ return file_correct; };

        /**
         * @brief Get the size object
         * 
         * @return int 
         */
        int get_size(){ return size; };

        /**
         * @brief Get the header object
         * 
         * @return vector<string> 
         */
        vector<string> get_header(){ return header; };

        /**
         * @brief Get the paths object
         * 
         * @return vector<vector<int>> 
         */
        vector<vector<int>> get_paths() { return paths; };

        /**
         * @brief Get the names object
         * 
         * @return vector<vector<string>> 
         */
        vector<vector<string>> get_names(){ return names; };

        /**
         * @brief Get the coefficient object
         * 
         * @return float 
         */
        float get_coefficient();

        /**
         * @brief Output operator
         * 
         * @param ostr 
         * @param dag 
         * @return ostream& 
         */
        friend ostream& operator<<(ostream& ostr, const DAG& dag);
};

/**
 * @brief Output operator
 * 
 * @param ostr 
 * @param dag 
 * @return ostream& 
 */
ostream& operator<<(ostream& ostr, const DAG& dag);

#endif // DAG_H