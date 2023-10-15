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
const char* first;
const char* src;
int tok;
//

[[noreturn]] void err(string msg) {
	size_t line = 1;
	for (auto s = text.data(); s != src; ++s)
		if (*s == '\n')
			++line;
	throw runtime_error(file + ':' + to_string(line) + ": " + msg);
}

[[noreturn]] void err(const char* s, string msg) {
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
		case '\'': {
			auto s = src + 1;
			while (*s != '\'') {
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
	auto n = strlen(t);
	if (src - first != n)
		return 0;
	if (_memicmp(first, t, n))
		return 0;
	lex();
	return 1;
}

void expect(char k) {
	if (!eat(k))
		err(string("expected '") + k + '\'');
}

struct Table {
	const char* first;
	string name;
	vector<pair<const char*, string>> refs;
	vector<Table*> links;

	Table(const char* first, string name): first(first), name(name) {
	}
};

vector<Table*> tables;

void parse() {
	while (tok) {
		if (!(eat("create") && eat("table"))) {
			lex();
			continue;
		}

		if (tok != k_word)
			err("expected name");
		auto table = new Table(first, {first, src});
		lex();

		expect('(');
		size_t depth = 1;
		while (depth) {
			switch (tok) {
			case '(':
				++depth;
				break;
			case ')':
				--depth;
				break;
			case 0:
				err(table->first, "unclosed CREATE TABLE");
			case k_word:
				if (eat("references")) {
					if (tok != k_word)
						err("expected name");
					table->refs.emplace_back(first, string(first, src));
				}
				break;
			}
			lex();
		}
		expect(';');

		tables.push_back(table);
		continue;
	}
}

// resolve names to pointers to tables
void link() {
	unordered_map<string, Table*> m;
	for (auto table: tables) {
		auto& t = m[table->name];
		if (t)
			err(table->first, table->name + ": duplicate name");
		t = table;
	}

	for (auto table: tables)
		for (auto& r: table->refs) {
			auto t = m[r.second];
			if (!t)
				err(r.first, r.second + ": not found");
			table->links.push_back(t);
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
