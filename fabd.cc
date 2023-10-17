#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <algorithm>
#include <exception>
#include <fstream>
#include <iostream>
#include <iterator>
#include <ostream>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>
using namespace std;

#ifdef NDEBUG
#define debug(a)
#else
#define debug(a) cout << __FILE__ << ':' << __LINE__ << ": " << __func__ << ": " << #a << ": " << (a) << '\n'
#endif

string file;
string text;

bool isid(unsigned char c) {
	return isalnum(c) || c == '_';
}

// tokenizer
enum {
	k_quote = 0x100,
	k_word,
};

// SORT
char* first;
char* src;
int tok;
//

[[noreturn]] void err(string msg) {
	size_t line = 1;
	for (auto s = text.data(); s != src; ++s)
		if (*s == '\n')
			++line;
	throw runtime_error(file + ':' + to_string(line) + ": " + msg);
}

[[noreturn]] void err(char* s, string msg) {
	src = s;
	err(msg);
}

void lex() {
	for (;;) {
		first = src;
		switch (*src) {
		case ' ':
		case '\f':
		case '\n':
		case '\r':
		case '\t':
			++src;
			continue;
		case '"':
		case '\'': {
			auto q = *src;
			auto s = src + 1;
			while (*s != q) {
				switch (*s) {
				case '\\':
					s += 2;
					continue;
				case '\n':
					err("unclosed quote");
				}
				++s;
			}
			src = s + 1;
			tok = k_quote;
			return;
		}
		case '-':
			if (src[1] == '-') {
				src = strchr(src, '\n');
				continue;
			}
			break;
		case 'A':
		case 'B':
		case 'C':
		case 'D':
		case 'E':
		case 'F':
		case 'G':
		case 'H':
		case 'I':
		case 'J':
		case 'K':
		case 'L':
		case 'M':
		case 'N':
		case 'O':
		case 'P':
		case 'Q':
		case 'R':
		case 'S':
		case 'T':
		case 'U':
		case 'V':
		case 'W':
		case 'X':
		case 'Y':
		case 'Z':
		case '_':
		case 'a':
		case 'b':
		case 'c':
		case 'd':
		case 'e':
		case 'f':
		case 'g':
		case 'h':
		case 'i':
		case 'j':
		case 'k':
		case 'l':
		case 'm':
		case 'n':
		case 'o':
		case 'p':
		case 'q':
		case 'r':
		case 's':
		case 't':
		case 'u':
		case 'v':
		case 'w':
		case 'x':
		case 'y':
		case 'z': {
			auto s = src;
			do
				++s;
			while (isid(*s));
			src = s;
			tok = k_word;
			return;
		}
		case '[': {
			auto s = src + 1;
			while (*s != ']') {
				if (*s == '\n')
					err("unclosed '['");
				++s;
			}
			src = s + 1;
			tok = k_quote;
			return;
		}
		case '\\':
			src = strchr(src, '\n');
			continue;
		case 0:
			tok = 0;
			return;
		}
		tok = *src++;
		return;
	}
}

void initLex() {
	src = text.data();
	lex();
}

// parser
bool eat(int k) {
	if (tok != k)
		return 0;
	lex();
	return 1;
}

bool eat(const char* t) {
	if (tok != k_word)
		return 0;
	auto s = first;
	auto n = strlen(t);
	if (src - s != n)
		return 0;
	for (size_t i = 0; i != n; ++i)
		if (tolower((unsigned char)s[i]) != t[i])
			return 0;
	lex();
	return 1;
}

void expect(char k) {
	if (!eat(k))
		err(string("expected '") + k + '\'');
}

void expect(const char* t) {
	if (!eat(t))
		err(string("expected '") + t + '\'');
}

string word() {
	if (tok != k_word)
		err("expected word");
	string s(first, src);
	lex();
	return s;
}

string id() {
	string s;
	switch (tok) {
	case k_quote:
		s.assign(first + 1, src - 1);
		break;
	case k_word:
		s.assign(first, src);
		break;
	default:
		err("expected identifier");
	}
	lex();
	return s;
}

struct Table;

struct Field {
	string name;
	string type;

	// SORT
	bool key = 0;
	Field* linkField = 0;
	Table* linkTable = 0;
	bool nonull = 0;
	string refField;
	char* refFirst = 0;
	string refTable;
	bool serial = 0;
	//

	Field(string name): name(name) {
	}
};

void lower() {
	for (auto s = first; s != src; ++s)
		*s = tolower((unsigned char)*s);
}

Field* parseField() {
	auto field = new Field(id());
	lower();
	field->type = word();
	for (;;) {
		// SORT
		if (eat("generated")) {
			expect("always");
			expect("as");
			expect("identity");
			field->serial = 1;
			continue;
		}

		if (eat("not")) {
			expect("null");
			field->nonull = 1;
			continue;
		}

		if (eat("primary")) {
			expect("key");
			field->key = 1;
			continue;
		}

		if (eat("references")) {
			field->refFirst = first;
			field->refTable = id();
			expect('(');
			field->refField = id();
			expect(')');
			continue;
		}

		//
		break;
	}
	return field;
}

struct Table {
	string name;
	vector<Field*> fields;
	unordered_map<string, Field*> fieldsMap;
	vector<Table*> links;

	Table(string name): name(name) {
	}
};

vector<Table*> tables;

void semi() {
	auto s = first;
	while (!eat(';')) {
		if (!tok)
			err(s, "unfinished statement");
		lex();
	}
}

void parse() {
	while (tok) {
		if (eat("create")) {
			if (eat("database")) {
				semi();
				continue;
			}

			expect("table");
			auto table = new Table(id());
			expect('(');
			do
				table->fields.push_back(parseField());
			while (eat(','));
			expect(')');
			expect(';');

			tables.push_back(table);
			continue;
		}
		err("expected CREATE");
	}
}

// resolve names to pointers
void link() {
	unordered_map<string, Table*> tablesMap;
	for (auto table: tables) {
		tablesMap[table->name] = table;
		for (auto field: table->fields)
			table->fieldsMap[field->name] = field;
	}

	for (auto table: tables)
		for (auto field: table->fields)
			if (field->refTable.size()) {
				auto t = tablesMap[field->refTable];
				if (!t)
					err(field->refFirst, field->refTable + ": not found");
				field->linkTable = t;
				table->links.push_back(t);

				auto f = t->fieldsMap[field->refField];
				if (!f)
					err(field->refFirst, field->refField + ": not found");
			}
}

template <class T> void topologicalSortRecur(const vector<T>& v, vector<T>& o, unordered_set<T>& visited, T a) {
	if (!visited.insert(a).second)
		return;
	for (auto b: a->links)
		topologicalSortRecur(v, o, visited, b);
	o.push_back(a);
}

template <class T> void topologicalSort(vector<T>& v) {
	unordered_set<T> visited;
	vector<T> o;
	for (auto a: v)
		topologicalSortRecur(v, o, visited, a);
	v = o;
}

int main(int argc, char** argv) {
	try {
		for (int i = 1; i < argc; ++i) {
			auto s = argv[i];
			if (*s == '-') {
				while (*s == '-')
					++s;
				switch (*s) {
				case 'V':
				case 'v':
					cout << "fabd version 0\n";
					return 0;
				case 'h':
					cout << "Usage: fabd [options] [schema file]\n";
					cout << '\n';
					cout << "-h  Show help\n";
					cout << "-V  Show version\n";
					return 0;
				}
				throw runtime_error(string(argv[i]) + ": unknown option");
			}
			if (file.size())
				throw runtime_error(string(s) + ": already specified schema file");
			file = s;
		}

		if (file.size()) {
			ifstream is(file, ios::binary);
			text = {istreambuf_iterator<char>(is), istreambuf_iterator<char>()};
		} else {
			file = "stdin";
			text = {istreambuf_iterator<char>(cin), istreambuf_iterator<char>()};
		}

		// make sure input ends with a newline, to simplify parser code
		if (text.empty() || text.back() != '\n')
			text += '\n';

		initLex();
		parse();
		if (tables.empty())
			throw runtime_error(file + ": no tables found");
		link();

		auto sorted = tables;
		sort(sorted.begin(), sorted.end(), [](const Table* a, const Table* b) { return a->name < b->name; });
		topologicalSort(sorted);

		string o;
		cout << o;
		return 0;
	} catch (exception& e) {
		cerr << e.what() << '\n';
		return 1;
	}
}
