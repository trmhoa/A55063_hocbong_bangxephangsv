#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#include <errno.h>

/* ========================================================================= */
/* ==================== ĐỊNH NGHĨA CẤU TRÚC VÀ HẰNG SỐ ===================== */
/* ========================================================================= */
#define ID_LEN 32
#define NAME_LEN 100
#define REASON_LEN 256
#define MIN_GPA 2.5       // (Giả định GPA tối thiểu)
#define MIN_CREDITS 15    // (Giả định Số tín chỉ tối thiểu)

typedef struct {
    char id[ID_LEN];
    char name[NAME_LEN];
    double gpa;
    int credits;
    int discipline;
    int inputIndex;
} Student;

typedef struct {
    Student *data;
    int size;
    int cap;
} StudentList;

typedef struct {
    int line;
    int isInvalid; 
    char id[ID_LEN];
    char reason[REASON_LEN];
} Reject;

typedef struct {
    Reject *data;
    int size;
    int cap;
} RejectList;

typedef struct {
    long comparisons;
    long moves;
    double seconds;
} SortStats;

typedef int (*CompareFn)(const Student*, const Student*);

/* ========================================================================= */
/* ==================== PHẦN MÃ TỪ STUDENT.C =============================== */
/* ========================================================================= */

void sl_init(StudentList *l) { l->data = NULL; l->size = l->cap = 0; }
void sl_free(StudentList *l) { free(l->data); sl_init(l); }
void sl_push(StudentList *l, const Student *s) {
    if (l->size == l->cap) {
        int nc = l->cap ? l->cap * 2 : 16;
        Student *p = (Student *)realloc(l->data, (size_t)nc * sizeof(Student));
        if (!p) { fprintf(stderr, "Het bo nho\n"); exit(1); }
        l->data = p; l->cap = nc;
    }
    l->data[l->size++] = *s;
}

int sl_copy(StudentList *dst, const StudentList *src) {
    sl_free(dst);
    if (src->size == 0) return 0;
    dst->data = (Student *)malloc((size_t)src->size * sizeof(Student));
    if (!dst->data) return -1;
    memcpy(dst->data, src->data, (size_t)src->size * sizeof(Student));
    dst->size = dst->cap = src->size;
    return 0;
}

void rl_init(RejectList *l) { l->data = NULL; l->size = l->cap = 0; }
void rl_free(RejectList *l) { free(l->data); rl_init(l); }
void rl_push(RejectList *l, const Reject *r) {
    if (l->size == l->cap) {
        int nc = l->cap ? l->cap * 2 : 16;
        Reject *p = (Reject *)realloc(l->data, (size_t)nc * sizeof(Reject));
        if (!p) { fprintf(stderr, "Het bo nho\n"); exit(1); }
        l->data = p; l->cap = nc;
    }
    l->data[l->size++] = *r;
}

static char *trim(char *s) {
    while (isspace((unsigned char)*s)) s++;
    char *e = s + strlen(s);
    while (e > s && isspace((unsigned char)e[-1])) *--e = '\0';
    return s;
}

static int ieq(const char *a, const char *b) {
    while (*a && *b) {
        if (tolower((unsigned char)*a) != tolower((unsigned char)*b)) return 0;
        a++; b++;
    }
    return *a == *b;
}

static int split_csv(char *line, char *fields[], int max) {
    int n = 0; char *p = line;
    for (;;) {
        char *c = strchr(p, ',');
        if (n < max) fields[n] = p;
        n++;
        if (!c) break;
        *c = '\0'; p = c + 1;
    }
    return n;
}

static int parse_double(const char *s, double *out) {
    char *end; errno = 0;
    if (*s == '\0') return 0;
    double v = strtod(s, &end);
    if (errno || *end != '\0') return 0;
    *out = v; return 1;
}

static int parse_int(const char *s, int *out) {
    char *end; errno = 0;
    if (*s == '\0') return 0;
    long v = strtol(s, &end, 10);
    if (errno || *end != '\0' || v < -2147483647L || v > 2147483647L) return 0;
    *out = (int)v; return 1;
}

static void add_invalid(RejectList *inv, int line, const char *id, const char *reason) {
    Reject r; memset(&r, 0, sizeof r);
    r.line = line; r.isInvalid = 1;
    snprintf(r.id, ID_LEN, "%s", id ? id : "");
    snprintf(r.reason, REASON_LEN, "%s", reason);
    rl_push(inv, &r);
}

int load_csv(const char *path, StudentList *all, RejectList *invalid, int *dataLines) {
    FILE *f = fopen(path, "r");
    if (!f) return -1;
    sl_free(all); rl_free(invalid); *dataLines = 0;
    char buf[1024]; int lineNo = 0, seq = 0, firstNonEmpty = 1;

    while (fgets(buf, sizeof buf, f)) {
        lineNo++;
        if (!strchr(buf, '\n') && !feof(f)) {
            int ch; while ((ch = fgetc(f)) != '\n' && ch != EOF) {}
            (*dataLines)++;
            add_invalid(invalid, lineNo, "", "dong qua dai (>1023 ky tu)");
            firstNonEmpty = 0; continue;
        }
        char copy[1024]; snprintf(copy, sizeof copy, "%s", buf);
        char *line = trim(copy);
        if (*line == '\0') continue;
        if ((unsigned char)line[0] == 0xEF && (unsigned char)line[1] == 0xBB && (unsigned char)line[2] == 0xBF) line += 3;

        char *fld[8]; char tmp[1024]; snprintf(tmp, sizeof tmp, "%s", line);
        int n = split_csv(tmp, fld, 8);
        for (int i = 0; i < n && i < 8; i++) fld[i] = trim(fld[i]);

        if (firstNonEmpty) {
            firstNonEmpty = 0;
            if (n >= 1 && (ieq(fld[0], "id") == 0 || ieq(fld[0], "ma") == 0 || ieq(fld[0], "masv") == 0)) continue;
        }
        (*dataLines)++;

        if (n != 5) {
            char msg[REASON_LEN]; snprintf(msg, sizeof msg, "can 5 cot, dong co %d cot", n);
            add_invalid(invalid, lineNo, n >= 1 ? fld[0] : "", msg); continue;
        }

        Student s; memset(&s, 0, sizeof s); const char *err = NULL;
        if (fld[0][0] == '\0') err = "ma sinh vien rong";
        else if (strlen(fld[0]) >= ID_LEN) err = "ma sinh vien qua dai";
        else if (strlen(fld[1]) >= NAME_LEN) err = "ho ten qua dai";
        else if (!parse_double(fld[2], &s.gpa)) err = "GPA khong phai so";
        else if (s.gpa < 0.0 || s.gpa > 4.0) err = "GPA ngoai khoang [0,4]";
        else if (!parse_int(fld[3], &s.credits)) err = "tin chi khong phai so nguyen";
        else if (s.credits < 0) err = "tin chi am";
        else if (!parse_int(fld[4], &s.discipline)) err = "ky luat khong phai so nguyen";
        else if (s.discipline != 0 && s.discipline != 1) err = "ky luat phai la 0 hoac 1";

        if (!err) {
            for (int i = 0; i < all->size; i++)
                if (strcmp(all->data[i].id, fld[0]) == 0) { err = "ma sinh vien bi trung"; break; }
        }
        if (err) { add_invalid(invalid, lineNo, fld[0], err); continue; }

        snprintf(s.id, ID_LEN, "%s", fld[0]);
        snprintf(s.name, NAME_LEN, "%s", fld[1]);
        s.inputIndex = ++seq;
        sl_push(all, &s);
    }
    fclose(f); return 0;
}

void filter_eligible(const StudentList *all, StudentList *eligible, RejectList *ineligible) {
    sl_free(eligible); rl_free(ineligible);
    for (int i = 0; i < all->size; i++) {
        const Student *s = &all->data[i]; char why[REASON_LEN] = "";
        if (s->gpa < MIN_GPA - 1e-9) snprintf(why + strlen(why), sizeof why - strlen(why), "GPA %.2f < %.2f; ", s->gpa, MIN_GPA);
        if (s->credits < MIN_CREDITS) snprintf(why + strlen(why), sizeof why - strlen(why), "tin chi %d < %d; ", s->credits, MIN_CREDITS);
        if (s->discipline != 0) snprintf(why + strlen(why), sizeof why - strlen(why), "co vi pham ky luat; ");
        if (why[0] == '\0') sl_push(eligible, s);
        else {
            size_t L = strlen(why); if (L >= 2) why[L - 2] = '\0';
            Reject r; memset(&r, 0, sizeof r); r.line = 0; r.isInvalid = 0;
            snprintf(r.id, ID_LEN, "%s", s->id); snprintf(r.reason, REASON_LEN, "%s", why);
            rl_push(ineligible, &r);
        }
    }
}

int write_valid_csv(const char *path, const StudentList *e) {
    FILE *f = fopen(path, "w"); if (!f) return -1;
    fprintf(f, "id,name,gpa,credits,discipline\n");
    for (int i = 0; i < e->size; i++) fprintf(f, "%s,%s,%.2f,%d,%d\n", e->data[i].id, e->data[i].name, e->data[i].gpa, e->data[i].credits, e->data[i].discipline);
    fclose(f); return 0;
}

int write_rejected_csv(const char *path, const RejectList *inv, const RejectList *inel) {
    FILE *f = fopen(path, "w"); if (!f) return -1;
    fprintf(f, "type,line,id,reason\n");
    for (int i = 0; i < inv->size; i++) fprintf(f, "INVALID,%d,%s,%s\n", inv->data[i].line, inv->data[i].id, inv->data[i].reason);
    for (int i = 0; i < inel->size; i++) fprintf(f, "INELIGIBLE,,%s,%s\n", inel->data[i].id, inel->data[i].reason);
    fclose(f); return 0;
}

int write_ranked_csv(const char *path, const StudentList *r) {
    FILE *f = fopen(path, "w"); if (!f) return -1;
    fprintf(f, "rank,id,name,gpa,credits,discipline\n");
    for (int i = 0; i < r->size; i++) fprintf(f, "%d,%s,%s,%.2f,%d,%d\n", i + 1, r->data[i].id, r->data[i].name, r->data[i].gpa, r->data[i].credits, r->data[i].discipline);
    fclose(f); return 0;
}

/* ========================================================================= */
/* ==================== PHẦN MÃ TỪ SORTS.C ================================= */
/* ========================================================================= */

int cmp_main(const Student *a, const Student *b) {
    if (a->gpa > b->gpa) return -1;
    if (a->gpa < b->gpa) return  1;
    if (a->credits > b->credits) return -1;
    if (a->credits < b->credits) return  1;
    return strcmp(a->id, b->id);
}

int cmp_gpa_only(const Student *a, const Student *b) {
    if (a->gpa > b->gpa) return -1;
    if (a->gpa < b->gpa) return  1;
    return 0;
}

static double now_sec(void) { return (double)clock() / CLOCKS_PER_SEC; }

void insertion_sort(Student *a, int n, CompareFn cmp, SortStats *st) {
    st->comparisons = st->moves = 0; double t0 = now_sec();
    for (int i = 1; i < n; i++) {
        Student key = a[i];  st->moves++;
        int j = i - 1;
        while (j >= 0) {
            st->comparisons++;
            if (cmp(&key, &a[j]) < 0) { a[j + 1] = a[j]; st->moves++; j--; }
            else break;
        }
        a[j + 1] = key; st->moves++;
    }
    st->seconds = now_sec() - t0;
}

void selection_sort(Student *a, int n, CompareFn cmp, SortStats *st) {
    st->comparisons = st->moves = 0; double t0 = now_sec();
    for (int i = 0; i < n - 1; i++) {
        int best = i;
        for (int j = i + 1; j < n; j++) {
            st->comparisons++;
            if (cmp(&a[j], &a[best]) < 0) best = j;
        }
        if (best != i) {
            Student t = a[i]; a[i] = a[best]; a[best] = t; st->moves += 3;
        }
    }
    st->seconds = now_sec() - t0;
}

void bubble_sort(Student *a, int n, CompareFn cmp, SortStats *st) {
    st->comparisons = st->moves = 0; double t0 = now_sec();
    for (int i = 0; i < n - 1; i++) {
        int swapped = 0;
        for (int j = 0; j < n - 1 - i; j++) {
            st->comparisons++;
            if (cmp(&a[j + 1], &a[j]) < 0) {
                Student t = a[j]; a[j] = a[j + 1]; a[j + 1] = t;
                st->moves += 3; swapped = 1;
            }
        }
        if (!swapped) break;
    }
    st->seconds = now_sec() - t0;
}

int is_sorted(const Student *a, int n, CompareFn cmp) {
    for (int i = 0; i + 1 < n; i++) if (cmp(&a[i], &a[i + 1]) > 0) return 0;
    return 1;
}

int is_stable(const Student *a, int n, CompareFn cmp, const Student **b1, const Student **b2) {
    for (int i = 0; i + 1 < n; i++) {
        if (cmp(&a[i], &a[i + 1]) == 0 && a[i].inputIndex > a[i + 1].inputIndex) {
            if (b1) *b1 = &a[i]; if (b2) *b2 = &a[i + 1]; return 0;
        }
    }
    return 1;
}

/* ========================================================================= */
/* ==================== PHẦN MÃ TỪ MAIN.C ================================== */
/* ========================================================================= */

typedef enum { ALG_INSERTION = 1, ALG_SELECTION, ALG_BUBBLE } Alg;
typedef enum { CRIT_MAIN = 1, CRIT_GPA } Crit;

static const char *alg_name(Alg a) { return a == ALG_INSERTION ? "Insertion Sort" : a == ALG_SELECTION ? "Selection Sort" : "Bubble Sort"; }
static const char *crit_name(Crit c) { return c == CRIT_MAIN ? "Xep hang chinh (GPA giam, tin chi giam, ma SV tang)" : "Chi GPA giam dan"; }
static CompareFn crit_fn(Crit c) { return c == CRIT_MAIN ? cmp_main : cmp_gpa_only; }

static void run_sort(Alg a, Student *arr, int n, CompareFn cmp, SortStats *st) {
    if (a == ALG_INSERTION) insertion_sort(arr, n, cmp, st);
    else if (a == ALG_SELECTION) selection_sort(arr, n, cmp, st);
    else bubble_sort(arr, n, cmp, st);
}

static char       g_path[256] = "students.csv";
static StudentList g_all, g_eligible, g_ranked;
static RejectList  g_invalid, g_ineligible;
static int  g_dataLines = 0, g_loaded = 0, g_filtered = 0, g_sorted = 0;
static Crit g_crit = CRIT_MAIN;
static Alg  g_alg  = ALG_INSERTION;
static SortStats g_stats;
static Alg  g_lastAlg; static Crit g_lastCrit;
static int  g_lastStable = 1, g_lastSorted = 1;

static int read_int(const char *prompt) {
    char buf[64]; printf("%s", prompt);
    if (!fgets(buf, sizeof buf, stdin)) return -1;
    char *end; long v = strtol(buf, &end, 10);
    if (end == buf) return -1;
    return (int)v;
}

static void print_student_row(int rank, const Student *s) {
    printf("  %-5d %-8s %-14s %5.2f %7d %5d  (nhap #%d)\n", rank, s->id, s->name, s->gpa, s->credits, s->discipline, s->inputIndex);
}

static void do_load(void) {
    char buf[256]; printf("Duong dan CSV [%s]: ", g_path);
    if (fgets(buf, sizeof buf, stdin)) {
        buf[strcspn(buf, "\r\n")] = '\0';
        if (buf[0]) snprintf(g_path, sizeof g_path, "%s", buf);
    }
    if (load_csv(g_path, &g_all, &g_invalid, &g_dataLines) != 0) {
        printf("Khong mo duoc file '%s'.\n", g_path); g_loaded = g_filtered = g_sorted = 0; return;
    }
    g_loaded = 1; g_filtered = g_sorted = 0;
    printf("Da doc %d dong du lieu: %d hop le ve dinh dang, %d loi.\n", g_dataLines, g_all.size, g_invalid.size);
    for (int i = 0; i < g_invalid.size; i++)
        printf("  [Loi] dong %d (%s): %s\n", g_invalid.data[i].line, g_invalid.data[i].id[0] ? g_invalid.data[i].id : "?", g_invalid.data[i].reason);
}

static int ensure_filtered(void) {
    if (!g_loaded) { printf("Hay doc CSV truoc (muc 1).\n"); return 0; }
    if (!g_filtered) { filter_eligible(&g_all, &g_eligible, &g_ineligible); g_filtered = 1; g_sorted = 0; }
    return 1;
}

static void do_filter(void) {
    if (!g_loaded) { printf("Hay doc CSV truoc (muc 1).\n"); return; }
    filter_eligible(&g_all, &g_eligible, &g_ineligible); g_filtered = 1; g_sorted = 0;
    printf("Du dieu kien: %d | Khong du dieu kien: %d\n", g_eligible.size, g_ineligible.size);
    for (int i = 0; i < g_ineligible.size; i++) printf("  [Loai] %s: %s\n", g_ineligible.data[i].id, g_ineligible.data[i].reason);
}

static void do_choose_crit(void) {
    printf("1. %s\n2. %s\n", crit_name(CRIT_MAIN), crit_name(CRIT_GPA));
    int c = read_int("Chon tieu chi: ");
    if (c == 1 || c == 2) g_crit = (Crit)c; else printf("Lua chon khong hop le.\n");
    printf("Tieu chi hien tai: %s\n", crit_name(g_crit));
}

static void do_sort(void) {
    if (!ensure_filtered()) return;
    printf("1. Insertion Sort\n2. Selection Sort\n3. Bubble Sort\n");
    int a = read_int("Chon thuat toan: ");
    if (a < 1 || a > 3) { printf("Lua chon khong hop le.\n"); return; }
    g_alg = (Alg)a;
    if (sl_copy(&g_ranked, &g_eligible) != 0) { printf("Het bo nho.\n"); return; }
    CompareFn cmp = crit_fn(g_crit);
    run_sort(g_alg, g_ranked.data, g_ranked.size, cmp, &g_stats);
    g_sorted = 1; g_lastAlg = g_alg; g_lastCrit = g_crit;
    g_lastSorted = is_sorted(g_ranked.data, g_ranked.size, cmp);
    const Student *b1 = NULL, *b2 = NULL;
    g_lastStable = is_stable(g_ranked.data, g_ranked.size, cmp, &b1, &b2);
    printf("%s | %s\n", alg_name(g_alg), crit_name(g_crit));
    printf("  So sanh: %ld | Di chuyen: %ld | Thoi gian: %.6f s\n", g_stats.comparisons, g_stats.moves, g_stats.seconds);
    printf("  Dung thu tu: %s | On dinh: %s\n", g_lastSorted ? "DUNG" : "SAI", g_lastStable ? "CO" : "KHONG");
    if (!g_lastStable) printf("  Vi pham on dinh: %s (nhap #%d) dung truoc %s (nhap #%d) du bang khoa.\n", b1->id, b1->inputIndex, b2->id, b2->inputIndex);
}

static void do_topk(void) {
    if (!g_sorted) { printf("Hay xep hang truoc (muc 4).\n"); return; }
    int k = read_int("Nhap K: ");
    if (k < 0) { printf("K khong hop le.\n"); return; }
    if (k == 0) { printf("K = 0: khong co sinh vien nao de hien thi.\n"); }
    else {
        int show = k > g_ranked.size ? g_ranked.size : k;
        if (k > g_ranked.size) printf("K = %d lon hon so hop le (%d): chi in %d sinh vien.\n", k, g_ranked.size, show);
        printf("  Hang  Ma SV    Ho ten           GPA  TinChi KyLuat\n");
        for (int i = 0; i < show; i++) print_student_row(i + 1, &g_ranked.data[i]);
    }
    printf("Thong ke: tong %d | loi dinh dang %d | khong du dieu kien %d | du dieu kien %d\n", g_dataLines, g_invalid.size, g_ineligible.size, g_eligible.size);
}

static void do_compare_all(void) {
    if (!ensure_filtered()) return;
    CompareFn cmp = crit_fn(g_crit);
    printf("Tieu chi: %s | n = %d\n", crit_name(g_crit), g_eligible.size);
    printf("  %-16s %12s %12s %12s %8s %8s\n", "Thuat toan", "So sanh", "Di chuyen", "Thoi gian(s)", "Dung", "On dinh");
    for (int a = 1; a <= 3; a++) {
        StudentList tmp; sl_init(&tmp); sl_copy(&tmp, &g_eligible);
        SortStats st; run_sort((Alg)a, tmp.data, tmp.size, cmp, &st);
        printf("  %-16s %12ld %12ld %12.6f %8s %8s\n", alg_name((Alg)a), st.comparisons, st.moves, st.seconds, is_sorted(tmp.data, tmp.size, cmp) ? "DUNG" : "SAI", is_stable(tmp.data, tmp.size, cmp, NULL, NULL) ? "CO" : "KHONG");
        sl_free(&tmp);
    }
}

static void do_write(void) {
    if (!g_sorted) { printf("Hay xep hang truoc (muc 4).\n"); return; }
    int ok = 1;
    ok &= write_valid_csv("valid.csv", &g_eligible) == 0;
    ok &= write_rejected_csv("rejected.csv", &g_invalid, &g_ineligible) == 0;
    ok &= write_ranked_csv("ranked.csv", &g_ranked) == 0;
    FILE *f = fopen("summary.txt", "w");
    if (f) {
        time_t t = time(NULL);
        fprintf(f, "BAO CAO XET HOC BONG\n");
        fprintf(f, "Thoi diem chay      : %s", ctime(&t));
        fprintf(f, "Tep dau vao         : %s\n", g_path);
        fprintf(f, "Tong so dong du lieu: %d\n", g_dataLines);
        fprintf(f, "Loi dinh dang       : %d\n", g_invalid.size);
        fprintf(f, "Hop le ve dinh dang : %d\n", g_all.size);
        fprintf(f, "Khong du dieu kien  : %d\n", g_ineligible.size);
        fprintf(f, "Du dieu kien        : %d\n", g_eligible.size);
        fprintf(f, "Thuat toan          : %s\n", alg_name(g_lastAlg));
        fprintf(f, "Tieu chi            : %s\n", crit_name(g_lastCrit));
        fprintf(f, "So lan so sanh      : %ld\n", g_stats.comparisons);
        fprintf(f, "So lan di chuyen    : %ld\n", g_stats.moves);
        fprintf(f, "Thoi gian (s)       : %.6f\n", g_stats.seconds);
        fprintf(f, "Dung thu tu         : %s\n", g_lastSorted ? "DUNG" : "SAI");
        fprintf(f, "On dinh             : %s\n", g_lastStable ? "CO" : "KHONG");
        fclose(f);
    } else ok = 0;
    printf(ok ? "Da ghi valid.csv, rejected.csv, ranked.csv, summary.txt\n" : "Loi khi ghi file.\n");
}

static void print_menu(void) {
    printf("\n===== QUAN LY XET HOC BONG =====\n");
    printf("1. Doc CSV va kiem tra (F1)\n");
    printf("2. Loc sinh vien du dieu kien (F2)\n");
    printf("3. Chon tieu chi xep hang (F4)    [hien tai: %s]\n", crit_name(g_crit));
    printf("4. Xep hang bang thuat toan (F3)\n");
    printf("5. Hien thi Top K va thong ke (F5)\n");
    printf("6. Ghi ranked.csv, valid.csv, rejected.csv, summary.txt (F6)\n");
    printf("7. So sanh ca 3 thuat toan\n");
    printf("0. Thoat\n");
}

int main(int argc, char **argv) {
    if (argc > 1) snprintf(g_path, sizeof g_path, "%s", argv[1]);
    sl_init(&g_all); sl_init(&g_eligible); sl_init(&g_ranked);
    rl_init(&g_invalid); rl_init(&g_ineligible);

    for (;;) {
        print_menu();
        int c = read_int("Chon: ");
        if (c == 0 || c == -1) break;
        switch (c) {
            case 1: do_load(); break;
            case 2: do_filter(); break;
            case 3: do_choose_crit(); break;
            case 4: do_sort(); break;
            case 5: do_topk(); break;
            case 6: do_write(); break;
            case 7: do_compare_all(); break;
            default: printf("Lua chon khong hop le.\n");
        }
    }
    sl_free(&g_all); sl_free(&g_eligible); sl_free(&g_ranked);
    rl_free(&g_invalid); rl_free(&g_ineligible);
    return 0;
}