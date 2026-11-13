#include "../include/gfx.h"

extern bool debug_mode;
extern unsigned long best_scores[3];

WINDOW *setup_gfx(void)
{
	WINDOW *win = initscr();
	if (win == NULL) {
		perror("Couldn't create a window");
		exit(EXIT_FAILURE);
	}
	cbreak();
	noecho();
	clear();
	keypad(win, true);
	nodelay(win, true);
	curs_set(0);

	// Colors
	start_color();
	init_pair(0, COLOR_BLACK, COLOR_BLACK);
	init_pair(1, COLOR_MAGENTA, COLOR_MAGENTA);
	init_pair(2, COLOR_WHITE, COLOR_WHITE);
	init_pair(3, COLOR_BLUE, COLOR_BLUE);
	init_pair(4, COLOR_RED, COLOR_RED);
	init_pair(5, COLOR_GREEN, COLOR_GREEN);
	init_pair(6, COLOR_YELLOW, COLOR_YELLOW);

	return win;
}

void teardown_gfx(WINDOW *win)
{
	endwin();
}

static void draw_rect(int x1, int y1, int x2, int y2)
{
	mvhline(y1, x1, 0, x2 - x1);
	mvhline(y2, x1, 0, x2 - x1);
	mvvline(y1, x1, 0, y2 - y1);
	mvvline(y1, x2, 0, y2 - y1);
	mvaddch(y1, x1, ACS_ULCORNER);
	mvaddch(y2, x1, ACS_LLCORNER);
	mvaddch(y1, x2, ACS_URCORNER);
	mvaddch(y2, x2, ACS_LRCORNER);
}

void draw_grid_jewels(const struct grid *gr, int x, int y)
{
	for (int i = 0; i < gr->size; i++) {
		div_t d = div(i, gr->cols);
		int jwl = gr->cells[i];
		attron(COLOR_PAIR(jwl));
		mvaddch(y + d.quot, x + d.rem * 2, (jwl) ? jwl + '0' : ' ');
		mvaddch(y + d.quot, x + d.rem * 2 + 1, (jwl) ? jwl + '0' : ' ');
		attroff(COLOR_PAIR(jwl));
	}
}

void draw_piece(struct piece *pc, int offset_x, int offset_y)
{
	for (int i = 0; i < PIECE_SIZE; i++) {
		int y0 = pc->row + i;
		if (y0 >= -1) {
			enum color jwl = pc->colors[i];
			attron(COLOR_PAIR(jwl));
			mvaddch(offset_y + y0, offset_x + pc->col * 2,
				jwl + '0');
			mvaddch(offset_y + y0, offset_x + pc->col * 2 + 1,
				jwl + '0');
			attroff(COLOR_PAIR(jwl));
		}
	}
}

void draw_frame(struct game *gm, int offset_x, int offset_y)
{
	draw_rect(offset_x, offset_y, offset_x + gm->grid->cols * 2 + 1,
		  offset_y + gm->grid->rows + 1);
	int x = offset_x + (gm->grid->cols + 1) * 2;
	draw_rect(x, offset_y, x + 2 + 1, offset_y + 4);
	draw_rect(x, offset_y + 5, x + 20 + 1, offset_y + 13);
}

void draw_game(struct game *gm, int offset_x, int offset_y)
{
	struct grid *gr = gm->grid;

	draw_grid_jewels(gr, offset_x + 1, offset_y + 1);
	draw_piece(&gm->next_piece, offset_x + gr->cols * 2 + 1 + 2,
		   offset_y + 1);
}

void draw_stars(const uint8_t *result, struct grid *gr, int x, int y)
{
	for (int i = 0; i < gr->size; i++) {
		div_t d = div(i, gr->cols);
		if (result[i]) {
			mvaddch(y + d.quot + 1, x + d.rem * 2 + 1, '[');
			mvaddch(y + d.quot + 1, x + d.rem * 2 + 2, ']');
		}
	}
}

void draw_debug(struct game *g, int x, int y)
{
	struct piece pc = g->current_piece;

	if (g->status == GAME_PAUSED) {
		mvprintw(y++, x, "== PAUSED ==");
	} else {
		mvprintw(y++, x, "            ");
	}
	unsigned long best = 0;
	switch ((enum game_class)g->color_max) {
	case CLASS_NOVICE:
		mvprintw(y++, x, "class:  novice");
		best = best_scores[0];
		break;
	case CLASS_AMATEUR:
		mvprintw(y++, x, "class:  amateur");
		best = best_scores[1];
		break;
	case CLASS_PRO:
		mvprintw(y++, x, "class:  pro");
		best = best_scores[2];
		break;
	default:
		mvprintw(y++, x, "class:  ???");
		break;
	}
	mvprintw(y++, x, "level:  %hu ", (unsigned short)g->level);
	mvprintw(y++, x, "jewels: %lu ", (unsigned long)g->jewels_removed);
	mvprintw(y++, x, "score:  %lu ", (unsigned long)g->score);
	if (g->last_score > 0) {
		mvprintw(y++, x, "        +%lu     ", (unsigned long)g->last_score);
	} else {
		mvprintw(y++, x, "                 ");
	}
	mvprintw(y++, x, "best:   %lu ", best > g->score ? best : (unsigned long)g->score);
	y++;
	mvprintw(y++, x - 1, "Left Right  move");
	mvprintw(y++, x - 1, "Down        drop faster");
	mvprintw(y++, x - 1, "Up A / Z    turn jewels");
	mvprintw(y++, x - 1, "P pause     Q quit");
	if (debug_mode) {
		y++;
		mvprintw(y++, x, "grid size:\t%d x %d  ", g->grid->cols, g->grid->rows);
		mvprintw(y++, x, "piece coords:\t%d, %d  ", pc.col, pc.row);
		int idx = g->grid->cols * (pc.row + PIECE_SIZE) + pc.col;
		if (idx >= 0 && idx < g->grid->cols * g->grid->rows) {
			mvprintw(y++, x, "under piece:\t%d  ", g->grid->cells[idx]);
		} else {
			mvprintw(y++, x, "under piece:\tn/a");
		}
		mvprintw(y++, x, "piece status:\t%d  ", g->current_piece.status);
		mvprintw(y++, x, "tick:\t%d  ", g->tick);
	}
}
