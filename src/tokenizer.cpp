#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double meanC(NumericVector x) {
  int n = x.size();
  double total = 0;
  
  for(int i = 0; i < n; ++i) {
    total += x[i];
  }
  return total / n;
}

// [[Rcpp::export]]
NumericVector test1(NumericVector x) {
  int n = x.size();
  std::vector<int> tasks;
  std::vector<int>::iterator it;
  int i;
  
  // from x to tasks
  for(i = 0; i < n; ++i) {
    tasks.push_back(x[i]);
  }
  tasks.erase(tasks.begin());
  
  // make tasks2
  std::vector<int> tasks2;
  for(i = 0; i < n; ++i) {
    tasks2.push_back(0);
  }
  
  // from tasks to task2
  i = 0;
  for(it = tasks.begin(); it != tasks.end(); ++it) {
    tasks2[i] = *it;
    i++;
  }
  
  // from tasks2 to v
  NumericVector v(n);
  i = 0;
  for(it = tasks2.begin(); it != tasks2.end(); ++it) {
    v[i++] = *it;
  }
  
  // empty loop
  int ct_n_items = 0;
  for (i = 0; i < ct_n_items; ++i) {
    v[i] = 9;
  } 
  
  return v;
}

// [[Rcpp::export]]
NumericMatrix skipgramsC_orig(int x_len,
                              int ngram_size,
                              int max_skip) {
  // ------------------------------------------------------------------------
  // returns a matrix with:
  //  - number of row equal to the number of solutions
  //  - number of columns equal to ngram_size
  //  - in the cells 1-based positions in x
  // ------------------------------------------------------------------------
  
  // create intermediate data structures
  // two stacks (tasks and completed)
  // one buffer
  std::vector<int> tasks;
  std::vector<int> completed;
  int n_completed = 0;
  std::vector<int> ct_buffer;             // items in ct (=current task)
  for (int i = 0; i < ngram_size; ++i) {
    ct_buffer.push_back(0);
  }
  int ct_next_pos;                        // next input for ct
  int ct_remaining_skips;                 // remaining skip for ct
  int ct_n_items;                         // number of items in ct_buffer
  
  // -- add first task
  tasks.push_back(0);                     // number of items in task
  tasks.push_back(max_skip);              // number of skips left
  tasks.push_back(1);                     // next position in input (1-based)
  
  // -- while loop that processes tasks stack
  while (!tasks.empty()) {
    
    // -- read and pop first task in tasks stack
    ct_next_pos = tasks.back();
    tasks.pop_back();
    
    ct_remaining_skips = tasks.back();
    tasks.pop_back();
    
    ct_n_items = tasks.back();
    tasks.pop_back();
    
    for (int i = 0; i < ct_n_items; ++i) {
      ct_buffer[i] = tasks.back();
      tasks.pop_back();
    }
    
    // -- process this task
    if (ct_n_items == ngram_size) {       // if current task is complete
      n_completed += 1;
      for (int i = 0; i < ct_n_items; ++i) {
        completed.push_back(ct_buffer[i]);
      } 
    } else if (ct_next_pos <= x_len) {    // if current task can be expanded 
      // -- derive new task (b.1) postpone 'consuming' a first item 
      if (ct_n_items == 0) {
        tasks.push_back(0);               // number of items in task
        tasks.push_back(max_skip);        // number of skips left
        tasks.push_back(ct_next_pos + 1); // next position in input (1-based)
      // -- derive new task (b.2) add skip to task that already has items   
      } else if (ct_remaining_skips > 0) {
        for (int i = ct_n_items - 1; i >= 0; --i) {
          tasks.push_back(ct_buffer[i]);
        }       
        tasks.push_back(ct_n_items);             // number of items in task
        tasks.push_back(ct_remaining_skips - 1); // number of skips left
        tasks.push_back(ct_next_pos + 1);        // next pos in input (1-based)
      }
      // -- derive new task (a.) 'consume' current input item 
      tasks.push_back(ct_next_pos);            // consume current input item
      for (int i = ct_n_items - 1; i >= 0; --i) {
        tasks.push_back(ct_buffer[i]);
      }       
      tasks.push_back(ct_n_items + 1);         // number of items in task
      tasks.push_back(ct_remaining_skips);     // number of skips left
      tasks.push_back(ct_next_pos + 1);        // next pos in input (1-based)
      // --
    }
  }

  // -- build result, i.e. turn data in compelted into a NumericMatrix
  NumericMatrix mat(n_completed, ngram_size);  
  std::vector<int>::iterator it = completed.begin();
  for (int i = 0; i < n_completed; ++i) {
    for (int j = 0; j < ngram_size; ++j) {
      mat(i, j) = *it++;
    }
  }
  
  return mat;
}

// [[Rcpp::export]]
CharacterVector skipgramsC(CharacterVector x,
                           int ngram_size,
                           int max_skip,
                           CharacterVector sep) {
  // ------------------------------------------------------------------------
  // returns a matrix with:
  //  - number of row equal to the number of solutions
  //  - number of columns equal to ngram_size
  //  - in the cells 1-based positions in x
  // ------------------------------------------------------------------------
  
  // create intermediate data structures
  // two stacks (tasks and completed)
  // one buffer
  String sp = sep(0);
  int x_len = x.size();
  std::vector<int> tasks;
  std::vector<int> completed;
  int n_completed = 0;
  std::vector<int> ct_buffer;             // items in ct (=current task)
  for (int i = 0; i < ngram_size; ++i) {
    ct_buffer.push_back(0);
  }
  int ct_next_pos;                        // next input for ct
  int ct_remaining_skips;                 // remaining skip for ct
  int ct_n_items;                         // number of items in ct_buffer
  
  // -- add first task
  tasks.push_back(0);                     // number of items in task
  tasks.push_back(max_skip);              // number of skips left
  tasks.push_back(1);                     // next position in input (1-based)
  
  // -- while loop that processes tasks stack
  while (!tasks.empty()) {
    
    // -- read and pop first task in tasks stack
    ct_next_pos = tasks.back();
    tasks.pop_back();
    
    ct_remaining_skips = tasks.back();
    tasks.pop_back();
    
    ct_n_items = tasks.back();
    tasks.pop_back();
    
    for (int i = 0; i < ct_n_items; ++i) {
      ct_buffer[i] = tasks.back();
      tasks.pop_back();
    }
    
    // -- process this task
    if (ct_n_items == ngram_size) {       // if current task is complete
      n_completed += 1;
      for (int i = 0; i < ct_n_items; ++i) {
        completed.push_back(ct_buffer[i]);
      } 
    } else if (ct_next_pos <= x_len) {    // if current task can be expanded 
      // -- derive new task (b.1) postpone 'consuming' a first item 
      if (ct_n_items == 0) {
        tasks.push_back(0);               // number of items in task
        tasks.push_back(max_skip);        // number of skips left
        tasks.push_back(ct_next_pos + 1); // next position in input (1-based)
      // -- derive new task (b.2) add skip to task that already has items   
      } else if (ct_remaining_skips > 0) {
        for (int i = ct_n_items - 1; i >= 0; --i) {
          tasks.push_back(ct_buffer[i]);
        }       
        tasks.push_back(ct_n_items);             // number of items in task
        tasks.push_back(ct_remaining_skips - 1); // number of skips left
        tasks.push_back(ct_next_pos + 1);        // next pos in input (1-based)
      }
      // -- derive new task (a.) 'consume' current input item 
      tasks.push_back(ct_next_pos);            // consume current input item
      for (int i = ct_n_items - 1; i >= 0; --i) {
        tasks.push_back(ct_buffer[i]);
      }       
      tasks.push_back(ct_n_items + 1);         // number of items in task
      tasks.push_back(ct_remaining_skips);     // number of skips left
      tasks.push_back(ct_next_pos + 1);        // next pos in input (1-based)
      // --
    }
  }

  // -- build result, i.e. turn data in completed into a NumericMatrix
  CharacterVector sk_grams(n_completed);  
  std::vector<int>::iterator it = completed.begin();
  for (int i = 0; i < n_completed; ++i) {
    String s(x(*it++ - 1)); // j = 0
    for (int j = 1; j < ngram_size; ++j) {
      s += sp;
      s += x(*it++ - 1);
    }
    sk_grams(i) = s;
  }
  
  return sk_grams;
}

