#include <iostream>
#include <sstream>
#include <fstream>
#include <cstddef>
#include <cassert>
#include <memory>
#include <iterator>
#include <functional>
#include <algorithm>
#include <vector>
#include <string>
#include <map>

namespace my {
	enum class tag { uninit, nil, num, sym, cons, subr, expr };
	
	class object {
	public:
		class ptr {
		public:
			ptr() : tag_(my::tag::uninit), ptr_(nullptr) {}
			decltype(auto) operator==(ptr o) { return ptr_ == o.ptr_; }
			decltype(auto) operator!=(ptr o) { return ptr_ != o.ptr_; }
			explicit ptr(object* p) : tag_(p->tag()), ptr_(p) {}
			decltype(auto) tag() const { return tag_; }
			decltype(auto) get() { return (ptr_); }
			decltype(auto) get() const { return const_cast<object const*>(ptr_); }
			decltype(auto) operator->() { return (ptr_); }
			decltype(auto) operator->() const { return const_cast<object const*>(ptr_); }
			decltype(auto) operator*() { return *ptr_; }
			decltype(auto) operator*() const { return const_cast<object const&>(*ptr_); }
			operator bool() const { return static_cast<bool>(ptr_); }

		private:
			my::tag tag_;
			object* ptr_;
		};

		struct cons {
			ptr car_;
			ptr cdr_;
		};
		struct expr {
			ptr args_;
			ptr body_;
			ptr env_;
		};
		using subr = std::function<ptr (ptr)>;

		struct data {
			data() {}
			explicit data(int num) : num_{num} {}
			explicit data(std::string str) : str_{std::move(str)} {}
			explicit data(ptr car, ptr cdr) : cons_{car, cdr} {} 
			explicit data(ptr args, ptr body, ptr env) : expr_{args, body, env} {}
			explicit data(subr s) : subr_{s} {}

			int num_;
			std::string str_;
			cons cons_;
			expr expr_;
			subr subr_;
		};

		object(my::tag l) : tag_(l) {}
		object(my::tag l, data d) : tag_(l), data_(d) {}

		my::tag tag() const { return tag_; }
		decltype(auto) is_marked() const { return is_marked_; }

		decltype(auto) mark() { is_marked_ = true; }
		decltype(auto) unmark() { assert(is_marked_); is_marked_ = false; }

		friend std::ostream& operator<<(std::ostream& os, object const& o) {
			if(o.tag() == my::tag::nil) {
				os << "nil";
			} else
			if(o.tag() == my::tag::num) {
				os << "num:" << o.data_.num_;
			} else
			if(o.tag() == my::tag::sym) {
				os << "sym:" << o.data_.str_;
			} else
			if(o.tag() == my::tag::cons) {
				auto list = my::object::ptr(const_cast<my::object*>(std::addressof(o)));
				auto first = true;
				std::ostringstream oss{};
				while(list.tag() == my::tag::cons) {
					if(first) {
						first = false;
					}
					else {
						oss << " ";
					}
					oss << *(list->data_.cons_.car_);
					list = list->data_.cons_.cdr_;
				}
				if(list.tag() == my::tag::nil) {
					os << "(" << oss.str() << ")";
				}
				else {
					os << "(" << oss.str() << " . " << *list << ")";
				}
			} else
			if(o.tag() == my::tag::subr) {
				assert(o.data_.subr_);
				os << "subr:" << o.data_.subr_.target_type().name();
			} else
			if(o.tag() == my::tag::expr) {
				os << "expr:"
					<< "args:" << *(o.data_.expr_.args_)
					<< "body:" << *(o.data_.expr_.body_)
					<< "env:" << o.data_.expr_.env_.get();
			}
			return os;
		}

	private:
		my::tag tag_;
		bool is_marked_{false};

	public:
		data data_;
	};

	namespace constant {
		auto nil = my::object{my::tag::nil};
		auto nilp = my::object::ptr(std::addressof(my::constant::nil));
	}// namespac econstant

	void mark(object::ptr obj) {
		if(obj->is_marked()) { return; }
		//std::cout << obj << " is marked" << std::endl;
		obj->mark();
		if(obj.tag() == my::tag::cons) {
			my::mark(obj->data_.cons_.car_);
			my::mark(obj->data_.cons_.cdr_);
		}else
		if(obj.tag() == my::tag::expr) {
			my::mark(obj->data_.expr_.args_);
			my::mark(obj->data_.expr_.body_);
			my::mark(obj->data_.expr_.env_);
		}
	}

	using sym_table = std::map<std::string, my::object::ptr>;

	decltype(auto) print(std::ostream& os, my::sym_table const& table) {
		os << "symbol table {\n";
		for(auto const& p : table) {
			os << "\t" << p.first << ":" << p.second.get() << "\n";
		}
		os << "}\n";
		return (os);
	}

	decltype(auto) sweep(
		std::vector<std::unique_ptr<my::object>>& objects,
		my::sym_table& sym_table
	) {
		for(std::unique_ptr<my::object>& e : objects) {
			/*
			std::cout << e.get() << " is " << 
				(e->is_marked() ? "marked" : "not marked") << std::endl;
			*/
			/*
			if(!e->is_marked()) {
				std::cout << e.get() << ":" << *e << " is not marked" << std::endl;
			}
			*/
			if(!e->is_marked() && e->tag() == my::tag::sym) {
				sym_table.erase(e->data_.str_);
			}
		}
		objects.erase(
			std::remove_if(objects.begin(), objects.end(), 
				[](auto& e) { return !e->is_marked(); }),
			objects.end()
		);
	}

	decltype(auto) safe_car(my::object::ptr o) {
		return o.tag() == my::tag::cons ? o->data_.cons_.car_ : my::constant::nilp;
	}

	decltype(auto) safe_cdr(my::object::ptr o) {
		return o.tag() == my::tag::cons ? o->data_.cons_.cdr_ : my::constant::nilp;
	}

	class storage {
	public:
		storage() {}

		decltype(auto) sweep_marked() {
			//std::cout << "sweep" << std::endl;
			my::sweep(objects_, sym_table_);

			{ // unmark
				my::constant::nil.unmark();
				for(auto& o : objects_) {
					o->unmark();
				}
			}
		}

		decltype(auto) create(my::object const& o) {
			objects_.push_back(std::make_unique<my::object>(o)); 
			return my::object::ptr(objects_.back().get());
		}

		decltype(auto) create_num(int num) {
			return create(my::object{my::tag::num, my::object::data{num}});
		}

		decltype(auto) create_sym(std::string str) {
			if(str=="nil") { return my::constant::nilp; }
			auto iter = sym_table_.find(str);
			if(iter == sym_table_.end()) {
				auto sym = create(
					my::object{my::tag::sym, my::object::data{std::move(str)}});
				sym_table_[sym->data_.str_] = sym;
				return sym;
			}
			else {
				return iter->second;
			}
		}

		decltype(auto) create_cons(object::ptr car, object::ptr cdr) {
			return create(my::object{my::tag::cons, my::object::data{car, cdr}});
		}

		decltype(auto) create_subr(my::object::subr s) {
			return create(my::object{my::tag::subr, my::object::data{s}});
		}

		decltype(auto) create_expr(
			my::object::ptr args, my::object::ptr body, my::object::ptr env
		) {
			return create(my::object{my::tag::expr, my::object::data{args, body, env}});
		}

		friend std::ostream& operator<<(std::ostream& os, storage const& s) {
			my::print(os, s.sym_table_);
			for(auto const& o : s.objects_) {
				os << o.get() << ":" << *o << "\n";
			}
			//print(os, const_cast<my::object::ptr>(std::addressof(s.env_)));
			return os;
		}

	private:
		std::vector<std::unique_ptr<my::object>> objects_;
		my::sym_table sym_table_;
	};

	decltype(auto) make_num(int n, my::storage& s) {
		return s.create_num(n);
	}

	decltype(auto) make_sym(std::string const& str, my::storage& s) {
		return str == "nil" ? my::constant::nilp : s.create_sym(str);
	}

	decltype(auto) make_cons(object::ptr car, object::ptr cdr, my::storage& s) {
		return s.create_cons(car, cdr);
	}

	decltype(auto) make_subr(my::object::subr subr, my::storage& s) {
		return s.create_subr(subr);
	}

	decltype(auto) make_expr(
		my::object::ptr args, my::object::ptr body, my::object::ptr env, my::storage& s
	) {
		return s.create_expr(args, body, env);
	}

	decltype(auto) find_var(my::object::ptr sym, my::object::ptr env) {
		while(env.tag() == my::tag::cons) {
			auto alist = env->data_.cons_.car_;
			while(alist.tag() == my::tag::cons) {
				if(alist->data_.cons_.car_->data_.cons_.car_ == sym) {
					return alist->data_.cons_.car_;
				}
				alist = alist->data_.cons_.cdr_;
			}
			env = env->data_.cons_.cdr_;
		}
		return my::constant::nilp;
	}
	
	decltype(auto) add_to_env(
		my::object::ptr sym, my::object::ptr val, my::object::ptr env, my::storage& s
	) {
		env->data_.cons_.car_ =
			my::make_cons(my::make_cons(sym, val, s), env->data_.cons_.car_, s);
	}
	
	decltype(auto) update_or_add_to_env(
		my::object::ptr sym, my::object::ptr val, my::object::ptr env, my::storage& s
	) {
		auto bind = my::find_var(sym, env);
		if(bind != my::constant::nilp) {
			bind->data_.cons_.cdr_ = val;
		}
		else {
			my::add_to_env(sym, val, env, s);
		}
	}


	decltype(auto) is_space(char c) {
		return c == '\t' || c == '\r' || c == '\n' || c == ' ';
	}
	
	namespace token {
		constexpr auto left_par = '(';
		constexpr auto right_par = ')';
		constexpr auto quote = '\'';
	}// namespace token

	decltype(auto) is_delimiter(char c) {
		return c == my::token::left_par || 
			c == my::token::right_par || 
			c == my::token::quote || 
			my::is_space(c);
	}

	decltype(auto) make_num_or_sym(std::string const& str, my::storage& s) {
		try {
			std::size_t pos;
			auto num = std::stoi(str, std::addressof(pos));
			return pos == str.size() ? my::make_num(num, s) : my::make_sym(str, s);
		}
		catch(std::invalid_argument) {
			return my::make_sym(str, s);
		}
		catch(std::out_of_range) {
			throw "too big num"; //TODO
		}
	}
	
	decltype(auto) make_env(my::storage& s) {
		auto env = my::make_cons(my::constant::nilp, my::constant::nilp, s);
		my::add_to_env(my::make_sym("eq", s), my::make_subr([&s](auto args) {
				auto x = my::safe_car(args);
				auto y = my::safe_car(my::safe_cdr(args));
				if(x.tag() == my::tag::num && y.tag() == my::tag::num) {
					return x->data_.num_ == y->data_.num_ ?
						my::make_sym("t", s) : my::constant::nilp;
				}
				else {
					return x == y ? my::make_sym("t", s) : my::constant::nilp;
				}
			}, s
		), env, s);
		my::add_to_env(my::make_sym("neq", s), my::make_subr([&s](auto args) {
				auto x = my::safe_car(args);
				auto y = my::safe_car(my::safe_cdr(args));
				if(x.tag() == my::tag::num && y.tag() == my::tag::num) {
					return x->data_.num_ != y->data_.num_ ?
						my::make_sym("t", s) : my::constant::nilp;
				}
				else {
					return x != y ? my::make_sym("t", s) : my::constant::nilp;
				}
			}, s
		), env, s);
		my::add_to_env(my::make_sym("car", s), my::make_subr([](auto args) {
				return my::safe_car(my::safe_car(args));
			}, s
		), env, s);
		my::add_to_env(my::make_sym("cdr", s), my::make_subr([](auto args) {
				return my::safe_cdr(my::safe_car(args));
			}, s
		), env, s);
		my::add_to_env(my::make_sym("*", s), my::make_subr([&s](auto args) {
				auto ret = 1;
				while(args.tag() == my::tag::cons) {
					if(args->data_.cons_.car_.tag() != my::tag::num) {
						throw "invalid type";
					}
					ret *= args->data_.cons_.car_->data_.num_;
					args = args->data_.cons_.cdr_;
				}
				return my::make_num(ret, s);
			}, s
		), env, s);
		my::add_to_env(my::make_sym("+", s), my::make_subr([&s](auto args) {
				auto ret = 0;
				while(args.tag() == my::tag::cons) {
					if(args->data_.cons_.car_.tag() != my::tag::num) {
						throw "invalid type";
					}
					ret += args->data_.cons_.car_->data_.num_;
					args = args->data_.cons_.cdr_;
				}
				return my::make_num(ret, s);
			}, s
		), env, s);
		my::add_to_env(my::make_sym("-", s), my::make_subr([&s](auto args) {
				auto x = my::safe_car(args);
				auto y = my::safe_car(my::safe_cdr(args));
				if(x.tag() != my::tag::num || y.tag() != my::tag::num) {
					throw "invalid type";
				}
				return my::make_num(x->data_.num_ - y->data_.num_, s);
			}, s
		), env, s);
		my::add_to_env(my::make_sym("input", s), my::make_subr([&s](auto args) {
				std::cout << "input called" << std::endl;
				std::string str;
				std::cin >> str;
				std::cout << str.length();
				return my::make_num_or_sym(str, s);
			}, s
		), env, s);
		my::add_to_env(my::make_sym("print", s), my::make_subr([&s](auto args) {
				//std::cout << *my::safe_car(args) << std::endl;
				auto p = my::safe_car(args);
				if(p.tag() == my::tag::nil) { std::cout << "nil"; }
				else if(p.tag() == my::tag::num) { std::cout << p->data_.num_; }
				else if(p.tag() == my::tag::sym) { std::cout << p->data_.str_; }
				return my::constant::nilp;
			}, s
		), env, s);
		my::add_to_env(my::make_sym("atom", s), my::make_subr([&s](auto args) {
				return my::safe_car(args).tag() == my::tag::cons ?
					my::constant::nilp : my::make_sym("t", s);
			}, s
		), env, s);
		my::add_to_env(my::make_sym("cons", s), my::make_subr([&s](auto args) {
				return my::make_cons(
					my::safe_car(args), 
					my::safe_car(my::safe_cdr(args)), s);
			}, s
		), env, s);
		my::add_to_env(my::make_sym("system", s), my::make_subr([&s](auto args) {
				auto command = my::safe_car(args);
				if(command.tag() != my::tag::sym) {
					return my::constant::nilp;
				}
				system(command->data_.str_.c_str());
				return command;
			}, s
		), env, s);
		my::add_to_env(my::make_sym("t", s), my::make_sym("t", s), env, s);
		return env;
	}

	template<typename Iter>
	decltype(auto) skip_spaces(Iter first, Iter const& last) {
		while(my::is_space(*first) && first != last) {
			++first;
		}
		return first;
	}

	template<typename Iter>
	struct parse_result {
		std::remove_reference_t<Iter> current_;
		my::object::ptr obj_;
	};

	template<typename Iter>
	decltype(auto) make_parse_result(Iter current, my::object::ptr obj) {
		return my::parse_result<Iter>{std::move(current), obj};
	}

	template<typename Iter>
	my::parse_result<Iter> parse(Iter first, Iter const& last, my::storage& s);

	decltype(auto) nreverse(my::object::ptr o, my::storage& s) {
		auto ret = my::constant::nilp;
		while(o.tag() == my::tag::cons) {
			auto temp = o->data_.cons_.cdr_;
			o->data_.cons_.cdr_ = ret;
			ret = o;
			o = temp;
		}
		return ret;
	}

	template<typename Iter>
	decltype(auto) parse_list(Iter first, Iter const& last, my::storage& s) {
		auto ret = my::constant::nilp;
		while(true) {
			first = my::skip_spaces(first, last);
			if(first == last) {
				throw "unfinished parenthesis"; //TODO
			}
			if(*first == my::token::right_par) {
				break;
			}
			auto res = my::parse(std::forward<Iter>(first), last, s);
			first = res.current_;
			ret = my::make_cons(res.obj_, ret, s);
		}
		++first;
		return my::make_parse_result(std::move(first), my::nreverse(ret, s));
	}

	template<typename Iter>
	decltype(auto) parse_atom(Iter&& first, Iter const& last, my::storage& s) {
		auto current = first;
		while(!my::is_delimiter(*current)) {
			++current;
		}
		auto str = std::string{std::forward<Iter>(first), current};
		return my::make_parse_result(
			std::move(current), my::make_num_or_sym(std::move(str), s));
	}

	template<typename Iter>
	my::parse_result<Iter> parse(Iter first, Iter const& last, my::storage& s) {
		first = my::skip_spaces(std::move(first), last);
		if(first == last) {
			throw "parse error"; //TODO
		} else 
		if(*first == my::token::right_par) {
			throw "invalid syntax"; //TODO
		} else
		if(*first == my::token::left_par) {
			++first;
			return my::parse_list(std::move(first), last, s);
		} else
		if(*first == my::token::quote) {
			++first;
			auto res = my::parse(std::move(first), last, s);
			return my::make_parse_result(
				res.current_,
				my::make_cons(
					my::make_sym("quote", s), 
					my::make_cons(res.obj_, my::constant::nilp, s),
					s
				)
			);
		}
		else {
			return my::parse_atom(std::move(first), last, s);
		}
	}

	decltype(auto) parse_str(std::string const& str, my::storage& s) {
		return my::parse(str.begin(), str.end(), s);
	}

	my::object::ptr eval(my::object::ptr obj, my::object::ptr env, my::storage& s);

	decltype(auto) eval_list(my::object::ptr list, my::object::ptr env, my::storage& s) {
		auto ret = my::constant::nilp;
		while(list.tag() == my::tag::cons) {
			auto e = my::eval(list->data_.cons_.car_, env, s);
			ret = my::make_cons(e, ret, s);
			list = list->data_.cons_.cdr_;
		}
		return my::nreverse(ret, s);
	}

	decltype(auto) progn(my::object::ptr body, my::object::ptr env, my::storage& s) {
		auto ret = my::constant::nilp;
		while(body.tag() == my::tag::cons) {
			ret = my::eval(body->data_.cons_.car_, env, s);
			body = body->data_.cons_.cdr_;
		}
		return ret;
	}

	/*
	// tail call optimization
	decltype(auto) progn_tco(my::object::ptr body, my::object::ptr env, my::storage& s) {
		auto if_expr = body->data_.cons_.car_;
		auto cond = my::safe_car(my::safe_cdr(if_expr));
		auto true_case = my::safe_car(my::safe_cdr(my::safe_cdr(if_expr)));
		auto false_case = my::safe_car(my::safe_cdr(my::safe_cdr(if_expr)));

		while(my::eval(cond, env, s) != my::constant::nilp) {
			progn(body, env, s);
		}
	}
	*/

	decltype(auto) pairlis(my::object::ptr llist, my::object::ptr rlist, my::storage& s) {
		auto ret = my::constant::nilp;
		while(llist.tag() == my::tag::cons && rlist.tag() == my::tag::cons) {
			ret = my::make_cons(
				my::make_cons(llist->data_.cons_.car_, rlist->data_.cons_.car_, s),
				ret,
				s
			);
			llist = llist->data_.cons_.cdr_;
			rlist = rlist->data_.cons_.cdr_;
		}
		return my::nreverse(ret, s);
	}

	decltype(auto) apply(my::object::ptr func, my::object::ptr args, my::storage& s) {
		//std::cout << *func << std::endl;
		if(func.tag() == my::tag::subr) {
			return func->data_.subr_(args);
		} else
		if(func.tag() == my::tag::expr) {
			return my::progn(
				func->data_.expr_.body_,
				my::make_cons(
					my::pairlis(func->data_.expr_.args_, args, s),
					func->data_.expr_.env_, s), s);
		}
		throw "not function applied";
	}

	my::object::ptr eval(my::object::ptr obj, my::object::ptr env, my::storage& s) {
		if(obj.tag() == my::tag::nil || obj.tag() == my::tag::num) {
			return obj;
		}
		if(obj.tag() == my::tag::sym) {
			auto bind = my::find_var(obj, env);
			if(bind == my::constant::nilp) {
				throw "no value"; //TODO
			}
			return bind->data_.cons_.cdr_;
		}
		auto op = my::safe_car(obj);
		auto args = my::safe_cdr(obj);
		if(op->data_.str_ == "quote") {
			return my::safe_car(args);
		} else
		if(op->data_.str_  == "if") {
			return my::eval(my::safe_car(args), env, s) == my::constant::nilp ?
				my::eval(my::safe_car(my::safe_cdr(my::safe_cdr(args))), env, s) : 
				my::eval(my::safe_car(my::safe_cdr(args)), env, s);
		} else
		if(op->data_.str_  == "loop") {
			auto cond = my::safe_car(args);
			auto tasks = my::safe_cdr(args);
			auto res = my::constant::nilp;
			while(my::eval(cond, env, s) != my::constant::nilp) {
				res = my::progn(tasks, env, s);
			}
			return res;
		} else
		if(op->data_.str_ == "lambda") {
			auto lambda_args = my::safe_car(args);
			auto body = my::safe_cdr(args);
			return my::make_expr(lambda_args, body, env, s);
		} else
		if(op->data_.str_ == "defun") {
			auto sym = my::safe_car(args);
			auto lambda_args = my::safe_car(my::safe_cdr(args));
			auto body = my::safe_cdr(my::safe_cdr(args));
			auto expr = my::make_expr(lambda_args, body, env, s);
			my::update_or_add_to_env(sym, expr, env, s);
			return sym;
		} else
		if(op->data_.str_ == "setq") {
			auto val = my::eval(my::safe_car(my::safe_cdr(args)), env, s);
			auto sym = my::safe_car(args);
			if(sym.tag() != my::tag::sym) {
				throw "invalid argment"; //TODO
			}
			my::update_or_add_to_env(sym, val, env, s);
			return val;
		}
		return my::apply(my::eval(op, env, s), my::eval_list(args, env, s), s);
	}
}// namespace my

int main(int argc, const char* argv[]) {
	std::cout << "hello my lisp" << std::endl;
	auto storage = my::storage{};
	auto root_env = my::make_env(storage);
	if(argc != 1) {
		try {
			std::cout << "load " << argv[1] << std::endl;
			std::ifstream ifs{argv[1]};
			/*
			auto load_obj = my::parse(std::istreambuf_iterator<char>(ifs),
						 std::istreambuf_iterator<char>(), storage).obj_;
			*/
			auto code = std::string(std::istreambuf_iterator<char>(ifs),
						 std::istreambuf_iterator<char>());
			auto load_obj = my::parse_str(code, storage).obj_;
			std::cout << *load_obj << std::endl;
			std::cout << *my::eval(load_obj, root_env, storage) << std::endl;
		}
		catch(char const* e) {
			std::cout << e << std::endl;
		}
	}
	while(true) {
		auto line = std::string{};
		std::cout << "\n> ";
		std::getline(std::cin, line);
		try {
			if(line == "s") {
				std::cout << storage << std::endl;
			} else
			if(line == "env") {
				std::cout << root_env << ":" << *root_env  << std::endl;
			}
			else {
				auto input_obj = my::parse_str(line+"\n", storage).obj_;
				std::cout << "input obj: " << *input_obj << std::endl;
				auto obj = *my::eval(input_obj, root_env, storage);
				std::cout << obj << std::endl;
				my::mark(root_env);
				storage.sweep_marked();
			}
		}
		catch(const char* e) {
			std::cout << e << std::endl;
		}
	}
}
