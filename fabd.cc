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
#include <chrono>
#include <exception>
#include <fstream>
#include <iostream>
#include <iterator>
#include <ostream>
#include <random>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>
using namespace std;

using namespace chrono;
using namespace literals;

#ifdef NDEBUG
#define debug(a)
#else
#define debug(a) cout << __FILE__ << ':' << __LINE__ << ": " << __func__ << ": " << #a << ": " << (a) << '\n'
#endif

string file;
string text;

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

bool isid(unsigned char c) {
	return isalnum(c) || c == '_';
}

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
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
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
	char* first;
	string name;
	string type;

	// SORT
	bool autoinc = 0;
	bool key = 0;
	Field* linkField = 0;
	Table* linkTable = 0;
	bool nonull = 0;
	string refField;
	char* refFirst = 0;
	string refTable;
	//

	Field(char* first, string name): first(first), name(name) {
	}
};

void lower() {
	for (auto s = first; s != src; ++s)
		*s = tolower((unsigned char)*s);
}

Field* parseField() {
	auto s = first;
	auto field = new Field(s, id());
	lower();
	field->type = word();
	for (;;) {
		// SORT
		if (eat("generated")) {
			expect("always");
			expect("as");
			expect("identity");
			field->autoinc = 1;
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

	vector<string> data;

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

struct Separator {
	bool subsequent = 0;

	bool operator()() {
		auto a = subsequent;
		subsequent = 1;
		return a;
	}
};

default_random_engine rndEngine;

size_t rnd(size_t n) {
	uniform_int_distribution<size_t> d(0, n - 1);
	return d(rndEngine);
}

template <class T> const T& rnd(const vector<T>& v) {
	return v[rnd(v.size())];
}

string makeVal(const Table* table, size_t i, const Field* field) {
	assert(!field->autoinc);
	if (field->key) {
		string s(1, toupper((unsigned char)table->name[0]));
		s += to_string(i);
		return '\'' + s + '\'';
	}
	if (field->linkTable)
		return '\'' + rnd(field->linkTable->data) + '\'';

	// SORT
		return '\'' + table->name + ' ' + field->name + '\'';
	if (field->type == "bigint" || field->type == "integer" || field->type == "smallint")
	if (field->type == "date") {
		auto date = sys_days(2023y / 1 / 1) + days(rnd(365));
		year_month_day ymd(date);
		char s[13];
		sprintf_s(s, sizeof s, "'%04d-%02d-%02d'", (int)ymd.year(), (unsigned)ymd.month(), (unsigned)ymd.day());
		return s;
	}
	if (field->type == "decimal") {
		auto s = to_string(rnd(10)) + '.';
		for (size_t i = 2; i--;)
			s += '0' + (char)rnd(10);
		return s;
	}
	if (field->type == "text")
		return to_string(rnd(1000));
	//
	err(field->first, field->type + ": unknown type");
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

		// default to reading schema from stdin
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

		// parse the schema
		initLex();
		parse();
		if (tables.empty())
			throw runtime_error(file + ": no tables found");
		link();

		// topological sort to insert dependencies first
		auto sorted = tables;
		sort(sorted.begin(), sorted.end(), [](const Table* a, const Table* b) { return a->name < b->name; });
		topologicalSort(sorted);

		// detail tables should have more records
		unordered_map<const Table*, size_t> tableSize;
		for (auto table: tables) {
			if (table->name == "country")
				continue;
			size_t n = 1;
			for (auto field: table->fields)
				if (field->linkTable)
					n = max(n, tableSize[field->linkTable]);
			n *= 10;
			tableSize[table] = n;
		}

		cout << "\\set ON_ERROR_STOP true\n";
		cout << "BEGIN;\n";

		// make sure to avoid polluting a database that already contains data
		cout << "DO $$\n";
		cout << "BEGIN\n";
		for (auto table: tables)
			if (tableSize[table])
				cout << "IF EXISTS (SELECT 1 FROM " << table->name
					 << ") THEN RAISE EXCEPTION 'Database already contains data'; END IF;\n";
		cout << "END $$;\n";

		for (auto table: tables) {
			auto n = tableSize[table];
			for (size_t i = 0; i != n; ++i) {
				Separator separator;
			}
		}

		cout << "COMMIT;\n";
		return 0;
	} catch (exception& e) {
		cerr << e.what() << '\n';
		return 1;
	}
}
