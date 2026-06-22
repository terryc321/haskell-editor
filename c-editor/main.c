
/*

  

a fixed sized text editor - ascii only

ncurses recap

initscr();      // Initialize
endwin();       // Restore terminal
printw(...);    // Print text
mvprintw(y,x,); // Print at (y,x)
refresh();      // Update screen
clear();        // Clear screen
getch();        // Read one key
noecho();       // Don't echo typed keys
cbreak();       // Disable line buffering
keypad(stdscr, TRUE); // Enable arrow/function keys

curs_set(0);   // Hide cursor
curs_set(1);   // Normal cursor
curs_set(2);   // Very visible cursor (if supported)
mvaddch(y, x, ch);

*/

#include <stdio.h>
#include <stdlib.h>
#include <ncurses.h>

#define BUFFER_SIZE   1000000
#define LAST_CHAR    (BUFFER_SIZE - 1)

static int buffer[BUFFER_SIZE];
static int gap_left = 0;
static int gap_right = BUFFER_SIZE - 1;

static void clear_buffer();
static void insert_char(int ch);
static void delete_char();
static void move_left();
static void move_right();
static void get_contents();

static void clear_buffer(){
  int i = 0;
  for(i=0 ; i< BUFFER_SIZE ; i++){
    buffer[i] = 0;
  }
}

static void insert_char(int ch){
  if (gap_left < gap_right){
    buffer[gap_left] = ch;
    gap_left ++;
  }
}
static void delete_char(){
  if (gap_left > 0){
    gap_left --;
    buffer[gap_left] = 0;    
  }
}
static void move_left(){
  if (gap_left > 0){
    int ch = buffer[gap_left-1];
    buffer[gap_right] = ch;
    buffer[gap_left-1] = 0;    
    gap_left --;
    gap_right--;
  }
}

static void move_right(){
  if (gap_right >= (BUFFER_SIZE - 1)){
    return;
  }
  buffer[gap_left] = buffer[gap_right+1];
  buffer[gap_right+1] = 0;
  gap_left ++;
  gap_right++;  
}

static void get_contents(){
  // todo 
}

// mean in word wrap mode , previous line on view
static void previous_line(){
  
}

// mean in word wrap mode , previous line on view
static void next_line(){
  
}

static void page_up(){
}

static void page_down(){
}

static void begin_of_line(){
  
}
static void end_of_line(){
  
}

static void redo_display(int rows,int cols){
  // margin left
  int mg = 2;
  // index into buffer
  int i = 0;
  // tracks position on screen y x 
  int x = 0 , y = 0;
  // cursor x y
  int cx = 0 , cy = 0 ,cch = 0; 
  int ch ;
  x = mg;
  while(i < gap_left){
    ch = buffer[i];
    if (ch != '\n'){
      mvaddch(y,x,ch);
      if (i == (gap_left - 1)){
	cch = ch;
      }
      x++;
    }
    else {
      y++;
      x=mg;
    }
    i ++;
  }
  cx = x;
  cy = y;
  
  i = gap_right+1;
  while(i <= (BUFFER_SIZE-2)){
    ch = buffer[i];
    if (ch != '\n'){
      mvaddch(y,x,ch);
      if (i == (gap_left - 1)){
	cch = ch;
      }
      x++;
    }
    else {
      y++;
      x=mg;
    }
    i ++;
  }

  // move cursor blinky to row y , col x
  move(cy,cx);
  // place char on top?
  //mvaddch(cy,cx,cch);
    
  
}



int main(int argc,char **argv){
  clear_buffer();
  initscr();
  // Start ncurses
  cbreak();
  noecho();
  keypad(stdscr,TRUE);
  clear();
  curs_set(1);
  curs_set(2);
  
  //printw("Ready>");
  refresh();
  // Draw to the screen
  // how big is the terminal ? width height
  //
  int ch;
  while ((ch = getch()) != 'q') {
    clear();
    switch (ch) {
    case KEY_DC:
      // 
      break;
    case KEY_BACKSPACE:
      delete_char();
      break;
    case KEY_IC:
      // insert key
      break;
    case KEY_HOME:
      // start of line
      break;
    case KEY_END:
      // end of line
      break;            
    case KEY_NPAGE:
      // page down
      break;
    case KEY_PPAGE:
      // page up
      break;      
    case KEY_UP:
      // move cursor a line up ??
      break;
    case KEY_DOWN:
      // move cursor a line down ??
      break;
    case KEY_LEFT:
      // move cursor left - remember break
      move_left();
      break;

    case KEY_RIGHT:
      // move cursor right - remember break
      move_right();
      break;

    default:
      // insert character ch - remember brek
      insert_char(ch);
      break;
    }

    int rows, cols;
    getmaxyx(stdscr, rows, cols);
    redo_display(rows,cols);
    refresh();
  }
  endwin();
  // Restore terminal
  return 0;
}

