#ifndef TRAITS_H_
#define TRAITS_H_

#include <algorithm>
#include <array>
#include <bit>
#include <cassert>
#include <cstdint>
#include <functional>
#include <numeric>
#include <tuple>
#include <type_traits>
#include <utility>
#include <variant>

// high level design overview:

// void (), is_function_type => function_type
// method_name

// method_signature: is_method_id => method_id
// [method_name, function_type]

// method_implementation: is_behavior_implementation, behavior_implementation
// [method_id, callable]

// method: is_behavior, behavior
// [method_id, method_kernel, default_implementation]

// trait:
// [constraint, behavior...]

// impl:
// [behavior_implementation...]

// Method<>
// TRAITS_METHOD()

namespace traits {
struct undefined {};

inline namespace tuple {
template <std::size_t, typename Element>
struct structural_tuple_leaf {
    Element e{};
};

template <typename Seq, typename... Elements>
struct structural_tuple_impl;

template <std::size_t... Is, typename... Elements>
struct structural_tuple_impl<std::index_sequence<Is...>, Elements...>
    : structural_tuple_leaf<Is, Elements>... {
    constexpr structural_tuple_impl() noexcept = default;
    constexpr structural_tuple_impl(Elements... values) noexcept
        requires(sizeof...(Elements) > 0)
        : structural_tuple_leaf<Is, Elements>(values)... {}
};

template <typename... Elements>
struct structural_tuple
    : structural_tuple_impl<std::make_index_sequence<sizeof...(Elements)>, Elements...> {
    using base_type =
        structural_tuple_impl<std::make_index_sequence<sizeof...(Elements)>, Elements...>;
    using base_type::base_type;

    static constexpr std::integral_constant<std::size_t, sizeof...(Elements)> size{};
};

template <typename... Elements>
structural_tuple(Elements...) -> structural_tuple<Elements...>;

template <typename Element, std::size_t I>
constexpr auto get(const structural_tuple_leaf<I, Element>& m) -> const Element& {
    return m.e;
}
template <typename Element, std::size_t I>
constexpr auto get(structural_tuple_leaf<I, Element>& m) -> Element& {
    return m.e;
}
template <std::size_t I, typename Element>
constexpr auto get(const structural_tuple_leaf<I, Element>& m) -> const Element& {
    return m.e;
}
template <std::size_t I, typename Element>
constexpr auto get(structural_tuple_leaf<I, Element>& m) -> Element& {
    return m.e;
}

template <std::size_t I, typename T>
using structural_tuple_element =
    std::type_identity<std::remove_cvref_t<decltype(get<I>(std::declval<T const&>()))>>;

template <std::size_t I, typename T>
using structural_tuple_element_t = typename structural_tuple_element<I, T>::type;

template <typename Tuple, typename AdditionalElement>
struct structural_tuple_prepend;

template <typename... Elements, typename AdditionalElement>
struct structural_tuple_prepend<structural_tuple<Elements...>, AdditionalElement>
    : std::type_identity<structural_tuple<AdditionalElement, Elements...>> {};

template <typename Tuple, typename RemovedElement>
struct structural_tuple_without;

template <typename RemovedElement>
struct structural_tuple_without<structural_tuple<>, RemovedElement>
    : std::type_identity<structural_tuple<>> {};

template <typename... Others, typename RemovedElement>
struct structural_tuple_without<structural_tuple<RemovedElement, Others...>, RemovedElement>
    : std::type_identity<structural_tuple<Others...>> {};

template <typename First, typename... Others, typename RemovedElement>
struct structural_tuple_without<structural_tuple<First, Others...>, RemovedElement> {
    using type = typename structural_tuple_prepend<
        typename structural_tuple_without<structural_tuple<Others...>, RemovedElement>::type,
        First>::type;
};

template <typename Tuple, typename RemovedElement>
using structural_tuple_without_t = typename structural_tuple_without<Tuple, RemovedElement>::type;

template <typename Tuple, typename OtherTuple>
struct structural_tuple_without_all_from;

template <typename Tuple>
struct structural_tuple_without_all_from<Tuple, structural_tuple<>> : std::type_identity<Tuple> {};

template <typename Tuple, typename Element, typename... OtherElements>
struct structural_tuple_without_all_from<Tuple, structural_tuple<Element, OtherElements...>>
    : structural_tuple_without_all_from<structural_tuple_without_t<Tuple, Element>,
                                        structural_tuple<OtherElements...>> {};

template <typename Tuple, typename OtherTuple>
using structural_tuple_without_all_from_t =
    typename structural_tuple_without_all_from<Tuple, OtherTuple>::type;

template <structural_tuple Tuple, structural_tuple OtherTuple>
concept subset_of =
    std::same_as<structural_tuple_without_all_from_t<decltype(Tuple), decltype(OtherTuple)>,
                 structural_tuple<>>;

template <structural_tuple Tuple, structural_tuple OtherTuple>
concept disjoint_from =
    std::same_as<structural_tuple_without_all_from_t<decltype(Tuple), decltype(OtherTuple)>,
                 decltype(Tuple)>;
}  // namespace tuple

inline namespace type_traits {
template <std::size_t Index, typename... Types>
struct nth_available : std::false_type {};

template <std::size_t Index, typename First, typename... Others>
    struct nth_available<Index, First, Others...> : std::integral_constant < bool,
    Index<1 + sizeof...(Others)> {};

template <std::size_t Index, typename... Types>
    requires(nth_available<Index, Types...>::value)
using nth = std::tuple_element<Index, std::tuple<Types...>>;

template <std::size_t Index, typename... Types>
using nth_t = typename nth<Index, Types...>::type;
}  // namespace type_traits

inline namespace functional {
class target;

template <typename... Ts>
struct overload : Ts... {
    using Ts::operator()...;
};
template <typename... Ts>
overload(Ts...) -> overload<Ts...>;

template <typename T>
struct is_function_type : std::false_type {};

template <typename Ret, typename... Args>
struct is_function_type<Ret(Args...)> : std::true_type {};

template <typename Ret, typename... Args>
struct is_function_type<Ret(Args...) const> : std::true_type {};

template <typename T>
concept function_type = is_function_type<T>::value;

template <function_type FunctionType>
struct canonical_function : std::type_identity<FunctionType> {};

template <typename Ret, typename... Args>
struct canonical_function<Ret(target&, Args...)> : std::type_identity<Ret(Args...)> {};

template <typename Ret, typename... Args>
struct canonical_function<Ret(target const&, Args...)> : std::type_identity<Ret(Args...) const> {};

template <typename Ret, typename... Args>
struct canonical_function<Ret(target&, Args...) const> {};

template <typename Ret, typename... Args>
struct canonical_function<Ret(target const&, Args...) const> {};

template <function_type FunctionType>
using canonical_function_t = typename canonical_function<FunctionType>::type;

template <typename T>
concept callable = requires { std::function{T{}}; };

template <typename T>
struct function_type_of;

template <typename Ret, typename... Args>
struct function_type_of<std::function<Ret(Args...)>> : std::type_identity<Ret(Args...)> {};

template <callable Callable>
struct function_type_of<Callable> : function_type_of<decltype(std::function{Callable{}})> {};

template <typename T>
using function_type_of_t = typename function_type_of<T>::type;

template <function_type ImplType>
struct method_from_implementation;

template <typename Ret, typename Object, typename... Args>
struct method_from_implementation<Ret(Object&, Args...)> : std::type_identity<Ret(Args...)> {};

template <typename Ret, typename Object, typename... Args>
struct method_from_implementation<Ret(Object const&, Args...)>
    : std::type_identity<Ret(Args...) const> {};

template <function_type ImplType>
using method_from_implementation_t = typename method_from_implementation<ImplType>::type;

template <callable CallableImplementation>
using function_type_from_implementation_t =
    method_from_implementation_t<function_type_of_t<CallableImplementation>>;

template <function_type FunctionType, typename Owner, template <typename> typename PassBy>
struct to_function_pointer;

template <typename Ret, typename... Args, typename Owner, template <typename> typename PassBy>
struct to_function_pointer<Ret(Args...), Owner, PassBy>
    : std::type_identity<Ret (*)(typename PassBy<Owner>::type, Args...)> {};

template <typename Ret, typename... Args, typename Owner, template <typename> typename PassBy>
struct to_function_pointer<Ret(Args...) const, Owner, PassBy>
    : std::type_identity<Ret (*)(typename PassBy<const Owner>::type, Args...)> {};

template <function_type FunctionType, typename Owner, template <typename T> typename PassBy>
using to_function_pointer_t = typename to_function_pointer<FunctionType, Owner, PassBy>::type;

template <function_type FunctionType>
struct constness;

template <typename Ret, typename... Args>
struct constness<Ret(Args...)> : std::false_type {};

template <typename Ret, typename... Args>
struct constness<Ret(Args...) const> : std::true_type {};

template <function_type FunctionType>
struct arity;

template <typename Ret, typename... Args>
struct arity<Ret(Args...)> : std::integral_constant<std::size_t, sizeof...(Args)> {};

template <typename Ret, typename... Args>
struct arity<Ret(Args...) const> : std::integral_constant<std::size_t, sizeof...(Args)> {};

template <bool Available, std::size_t Index, typename... Arguments>
struct nth_argument_impl : std::type_identity<undefined> {};

template <std::size_t Index, typename... Arguments>
struct nth_argument_impl<true, Index, Arguments...> : nth<Index, Arguments...> {};

template <std::size_t Index, function_type FunctionType>
struct nth_argument;

template <std::size_t Index, typename Ret, typename... Args>
struct nth_argument<Index, Ret(Args...)>
    : nth_argument_impl<nth_available<Index, Args...>::value, Index, Args...> {};

template <std::size_t Index, typename Ret, typename... Args>
struct nth_argument<Index, Ret(Args...) const>
    : nth_argument_impl<nth_available<Index, Args...>::value, Index, Args...> {};

template <std::size_t Index, function_type FunctionType>
using nth_argument_t = typename nth_argument<Index, FunctionType>::type;

template <std::size_t Index, function_type FunctionType>
constexpr auto forward_arg(std::remove_reference_t<nth_argument_t<Index, FunctionType>>& t) noexcept
    -> decltype(auto) {
    return static_cast<nth_argument_t<Index, FunctionType>&&>(t);
}

template <bool Constness, typename Ret, typename... Args>
struct make_function_type : std::type_identity<Ret(Args...)> {};

template <typename Ret, typename... Args>
struct make_function_type<true, Ret, Args...> : std::type_identity<Ret(Args...) const> {};

template <bool Constness, typename Ret, typename... Args>
using make_function_type_t = typename make_function_type<Constness, Ret, Args...>::type;

template <auto lambda, typename... Params>
struct generic_lambda_result
    : std::type_identity<decltype(lambda.template operator()<Params...>())> {};

template <auto lambda, typename... Params>
using generic_lambda_result_t = typename generic_lambda_result<lambda, Params...>::type;

template <std::size_t Size>
struct make_overload_set {
    auto operator()(auto&&...) const -> std::integral_constant<std::size_t, Size> { return {}; }
};

template <typename OverloadSet, std::size_t Index, function_type OverloadedType>
struct add_overload;

template <typename OverloadSet, std::size_t Index, typename Ret, typename... Args>
struct add_overload<OverloadSet, Index, Ret(Args...)> {
    struct type : OverloadSet {
        using OverloadSet::operator();
        auto operator()(Args...) -> std::integral_constant<std::size_t, Index> { return {}; }
    };
};

template <typename OverloadSet, std::size_t Index, typename Ret, typename... Args>
struct add_overload<OverloadSet, Index, Ret(Args...) const> {
    struct type : OverloadSet {
        using OverloadSet::operator();
        auto operator()(Args...) const -> std::integral_constant<std::size_t, Index> { return {}; }
    };
};

template <typename OverloadSet, std::size_t Index, function_type OverloadedType>
using add_overload_t = typename add_overload<OverloadSet, Index, OverloadedType>::type;

template <typename Object, function_type FunctionType, typename Impl>
struct make_callable;

template <typename Object, typename Ret, typename... Args, typename Impl>
struct make_callable<Object, Ret(Args...), Impl> {
    using type = decltype([](Object& object, Args... args) -> Ret {
        return std::invoke(Impl{}, object, std::forward<decltype(args)>(args)...);
    });
};

template <typename Object, typename Ret, typename... Args, typename Impl>
struct make_callable<Object, Ret(Args...) const, Impl> {
    using type = decltype([](Object const& object, Args... args) -> Ret {
        return std::invoke(Impl{}, object, std::forward<decltype(args)>(args)...);
    });
};

template <typename Object, function_type FunctionType, typename Impl>
using make_callable_t = typename make_callable<Object, FunctionType, Impl>::type;
}  // namespace functional

inline namespace methods {
template <std::size_t N>
    requires(N > 1)
struct method_name {
    constexpr method_name(const char (&str)[N]) noexcept {
        // FIXME: check for spaces
        // constexpr auto is_not_space = [] (const auto& letter) { return not std::isspace (letter);
        // };

        std::copy_n(str, N, name.data());
    }

    template <typename Ret, typename Object, typename... Args>
    constexpr auto operator=(std::invocable<Ret, Object const&, Args...> auto&& invocable) const;

    [[nodiscard]] constexpr auto begin() const { return name.data(); }
    [[nodiscard]] constexpr auto end() const { return name.data() + N; }

    std::array<char, N> name;
};
}  // namespace methods

inline namespace methods {
template <method_name MethodName, function_type FunctionType>
struct method_signature {
    static constexpr std::integral_constant<decltype(MethodName), MethodName> Name{};
    using function_type = FunctionType;
};

template <typename T>
struct is_method_id : std::false_type {};

template <method_name MethodName, function_type FunctionType>
struct is_method_id<method_signature<MethodName, FunctionType>> : std::true_type {};

template <typename T>
concept method_id = is_method_id<T>::value;

template <method_name MethodName, bool Constness, typename Ret, typename... Args>
using make_method =
    std::type_identity<method_signature<MethodName, make_function_type_t<Constness, Ret, Args...>>>;

template <method_name MethodName, bool Constness, typename Ret, typename... Args>
using make_method_t = typename make_method<MethodName, Constness, Ret, Args...>::type;

template <method_id Method, method_id... OtherMethods>
struct overloads;

template <method_id Method>
struct overloads<Method> : std::false_type {};

template <method_id Method, method_id CheckedMethod, method_id... OtherMethods>
struct overloads<Method, CheckedMethod, OtherMethods...> : overloads<Method, OtherMethods...> {};

template <method_name MethodName, function_type Signature1, function_type Signature2,
          method_id... OtherMethods>
struct overloads<method_signature<MethodName, Signature1>, method_signature<MethodName, Signature2>,
                 OtherMethods...> : std::true_type {};

template <method_name MethodName, typename OverloadSet, std::size_t Index, method_id... Methods>
struct viable_overloads;

template <method_name MethodName, typename OverloadSet, std::size_t Index>
struct viable_overloads<MethodName, OverloadSet, Index> : std::type_identity<OverloadSet> {};

template <method_name MethodName, typename OverloadSet, std::size_t Index,
          function_type OverloadedType, typename... OtherSignatures>
struct viable_overloads<MethodName, OverloadSet, Index,
                        method_signature<MethodName, OverloadedType>, OtherSignatures...>
    : viable_overloads<MethodName, add_overload_t<OverloadSet, Index, OverloadedType>, Index + 1,
                       OtherSignatures...> {};

template <method_name MethodName, typename OverloadSet, std::size_t Index,
          typename CheckedSignature, typename... OtherSignatures>
struct viable_overloads<MethodName, OverloadSet, Index, CheckedSignature, OtherSignatures...>
    : viable_overloads<MethodName, OverloadSet, Index + 1, OtherSignatures...> {};

template <method_name MethodName, method_id... Methods>
using viable_overloads_t =
    typename viable_overloads<MethodName, make_overload_set<sizeof...(Methods)>, 0,
                              Methods...>::type;
}  // namespace methods

inline namespace implementations {
constexpr auto get(...) -> structural_tuple<> { return {}; }

template <method_id Method, callable Callable>
struct method_implementation {
    using method_type = Method;
    Callable impl;
};

template <typename T>
struct is_behavior_implementation : std::false_type {};

template <method_id Method, callable Implementation>
struct is_behavior_implementation<method_implementation<Method, Implementation>> : std::true_type {
};

template <typename T>
concept behavior_implementation = is_behavior_implementation<T>::value;

template <method_id Method, typename T>
struct method_implementation_for {};

template <method_id Method, std::size_t Index = 0, behavior_implementation... Implementations>
constexpr auto impl_of(structural_tuple<Implementations...> const& implementations) noexcept {
    if constexpr (Index >= sizeof...(Implementations))
        return undefined{};
    else if constexpr (std::same_as<typename structural_tuple_element_t<
                                        Index, structural_tuple<Implementations...>>::method_type,
                                    Method>)
        return get<Index>(implementations).impl;
    else
        return impl_of<Method, Index + 1>(implementations);
}

template <method_id Method, typename T>
constexpr auto lookup_impl_for() noexcept {
    using namespace ::traits::implementations;
    return impl_of<Method>(get(method_implementation_for<Method, T>{}));
}

template <method_id Method, typename T>
constexpr auto is_method_implemented_for() noexcept {
    return not std::same_as<decltype(lookup_impl_for<Method, T>()), undefined>;
}
}  // namespace implementations

inline namespace constraints {
template <typename T>
concept constraint = requires(T t) {
    { t.template operator()<undefined>() } -> std::same_as<bool>;
    {
        std::bool_constant<(T{}.template operator()<undefined>(), true)>()
    } -> std::same_as<std::true_type>;
};

constexpr inline constraint auto Unconstrained = []<typename>() { return true; };
using unconstrained_type = std::remove_cv_t<decltype(Unconstrained)>;

template <constraint Constraint>
constexpr auto operator!(const Constraint&) noexcept {
    return []<typename T>() { return not Constraint{}.template operator()<T>(); };
}

template <constraint Constraint1, constraint Constraint2>
constexpr auto operator&&(const Constraint1&, const Constraint2&) noexcept {
    if constexpr (std::same_as<Constraint1, unconstrained_type>)
        return Constraint2{};
    else if constexpr (std::same_as<Constraint2, unconstrained_type>)
        return Constraint1{};
    else
        return []<typename T>() {
            return Constraint1{}.template operator()<T>() and
                   Constraint2{}.template operator()<T>();
        };
}

template <constraint Constraint1, constraint Constraint2>
constexpr auto operator||(const Constraint1&, const Constraint2&) noexcept {
    if constexpr (std::same_as<Constraint1, unconstrained_type> or
                  std::same_as<Constraint2, unconstrained_type>)
        return unconstrained_type{};
    else
        return []<typename T>() {
            return Constraint1{}.template operator()<T>() or Constraint2{}.template operator()<T>();
        };
}
}  // namespace constraints

inline namespace behaviors {
namespace behavior_detail {
template <method_id Method>
struct maybe_invoke {
    auto operator()(auto ptr, auto&&... args) const {
        using function_type = typename Method::function_type;

        constexpr bool ConstnessMatches =
            constness<function_type>::value ==
            std::is_const<std::remove_pointer_t<decltype(ptr)>>::value;
        constexpr bool ArityMatches = arity<function_type>::value == sizeof...(args);

        if constexpr (ConstnessMatches && ArityMatches)
            return ptr->template invoke<Method::Name()>(std::forward<decltype(args)>(args)...);
    }
};
}  // namespace behavior_detail

template <method_id Method, auto Kernel, typename DefaultImplementation = undefined>
struct method {
    using method_type = Method;
    using function_type = typename Method::function_type;

    static constexpr std::integral_constant<bool,
                                            not std::same_as<DefaultImplementation, undefined>>
        HasDefaultImplementation{};

    template <bool IsOverload, typename Type>
    using add_method_to =
        generic_lambda_result_t<Kernel, function_type, std::integral_constant<bool, IsOverload>,
                                Type, behavior_detail::maybe_invoke<Method>>;

    template <typename T, typename... Args>
    static constexpr auto invocable() {
        struct can_invoke {};
        return Kernel.template operator()<can_invoke{}, T, Args...>();
    }

    struct invoke_type {
        constexpr auto operator()(auto& object, auto&&... args) const {
            using object_type = std::remove_cvref_t<decltype(object)>;

            // try intrinic behavior first
            if constexpr (invocable<decltype(object), decltype(args)...>())
                return std::invoke(Kernel, std::forward<decltype(object)>(object),
                                   std::forward<decltype(args)>(args)...);

            // ... then try behavior impl
            else if constexpr (auto Impl = lookup_impl_for<Method, object_type>();
                               not std::same_as<decltype(Impl), undefined>)
                return std::invoke(Impl, std::forward<decltype(object)>(object),
                                   std::forward<decltype(args)>(args)...);

            // ... then try default implementation of behavior
            else if constexpr (not std::same_as<DefaultImplementation, undefined>)
                return std::invoke(DefaultImplementation{}, std::forward<decltype(object)>(object),
                                   std::forward<decltype(args)>(args)...);

            // ... or fail with compiler error message
            else
                return std::invoke(Kernel, std::forward<decltype(object)>(object),
                                   std::forward<decltype(args)>(args)...);
        }
    };

    template <typename Implementation>
    constexpr auto operator=(Implementation) const -> method<Method, Kernel, Implementation>
        requires std::same_as<DefaultImplementation, undefined>
    {
        return {};
    }
};

template <typename T>
struct is_behavior : public std::false_type {};

template <method_id Method, auto Kernel, typename Implementation>
struct is_behavior<method<Method, Kernel, Implementation>> : public std::true_type {};

template <typename T>
concept behavior = is_behavior<T>::value;

template <behavior Behavior>
struct has_default_implementation
    : std::integral_constant<bool, Behavior::HasDefaultImplementation()> {};

template <method_id CallerSignature, behavior... Behaviors>
struct invocable_method_index;

template <method_name MethodName, typename Ret, typename... Args, typename... Methods,
          auto... Kernels, typename... Implementations>
struct invocable_method_index<method_signature<MethodName, Ret(Args...)>,
                              method<Methods, Kernels, Implementations>...>
    : std::invoke_result_t<viable_overloads_t<MethodName, Methods...>, Args...> {};

template <method_name MethodName, typename Ret, typename... Args, typename... Methods,
          auto... Kernels, typename... Implementations>
struct invocable_method_index<method_signature<MethodName, Ret(Args...) const>,
                              method<Methods, Kernels, Implementations>...>
    : std::invoke_result_t<std::add_const_t<viable_overloads_t<MethodName, Methods...>>, Args...> {
};

template <behavior Behavior, behavior... OtherBehaviors>
struct behavior_overloads
    : overloads<typename Behavior::method_type, typename OtherBehaviors::method_type...> {};

template <typename Invocable, behavior... Behaviors>
struct with_behaviors;

template <typename Invocable>
struct with_behaviors<Invocable> : std::type_identity<Invocable> {};

template <typename Invocable, behavior FirstBehavior, behavior... OtherBehaviors>
struct with_behaviors<Invocable, FirstBehavior, OtherBehaviors...> {
    using type = typename FirstBehavior::template add_method_to<
        behavior_overloads<FirstBehavior, OtherBehaviors...>::value,
        typename with_behaviors<Invocable, OtherBehaviors...>::type>::type;
};

template <typename Invocable, behavior... Behaviors>
using with_behaviors_t = typename with_behaviors<Invocable, Behaviors...>::type;

template <typename Invocable, behavior... Behaviors>
struct with_immutable_behaviors;

template <typename Invocable>
struct with_immutable_behaviors<Invocable> : std::type_identity<Invocable> {};

template <typename Invocable, behavior FirstBehavior, behavior... OtherBehaviors>
struct with_immutable_behaviors<Invocable, FirstBehavior, OtherBehaviors...> {
    using type = typename FirstBehavior::template add_method_to<
        behavior_overloads<FirstBehavior, OtherBehaviors...>::value,
        std::add_const_t<typename with_immutable_behaviors<Invocable, OtherBehaviors...>::type>>::
        type;
};

template <typename Invocable, behavior... Behaviors>
using with_immutable_behaviors_t = typename with_immutable_behaviors<Invocable, Behaviors...>::type;

template <behavior Behavior, typename T>
struct is_behavior_defined_for
    : std::integral_constant<bool, is_method_implemented_for<typename Behavior::method_type, T>()> {
};

namespace behavior_detail {
template <typename T, behavior Behavior, function_type FunctionType>
    requires std::same_as<typename Behavior::function_type, FunctionType>
struct has_intrinsic_behavior_impl;

template <typename T, behavior Behavior, typename Ret, typename... Args>
struct has_intrinsic_behavior_impl<T, Behavior, Ret(Args...)>
    : std::integral_constant<bool, Behavior::template invocable<T&, Args...>()> {};

template <typename T, behavior Behavior, typename Ret, typename... Args>
struct has_intrinsic_behavior_impl<T, Behavior, Ret(Args...) const>
    : std::integral_constant<bool, Behavior::template invocable<T const&, Args...>()> {};
}  // namespace behavior_detail

template <typename T, behavior Behavior>
struct has_intrinsic_behavior
    : behavior_detail::has_intrinsic_behavior_impl<T, Behavior, typename Behavior::function_type> {
};

template <typename T, typename Behavior>
concept needs_impl_for = not has_default_implementation<Behavior>::value and
                         not has_intrinsic_behavior<T, Behavior>::value;

template <typename T, typename Behavior>
concept has_behavior =
    has_default_implementation<Behavior>::value or has_intrinsic_behavior<T, Behavior>::value or
    is_behavior_defined_for<Behavior, T>::value;
}  // namespace behaviors

inline namespace the_trait {
template <constraint Constraint, behavior... Behaviors>
struct trait : structural_tuple<Behaviors...> {
    constexpr trait() noexcept = default;
    constexpr trait(Behaviors...) noexcept
        requires(sizeof...(Behaviors) > 0)
    {}
    constexpr trait(Constraint, Behaviors...) noexcept {}

    using constraint_type = Constraint;
};

template <behavior... Behaviors>
trait(Behaviors...) -> trait<unconstrained_type, Behaviors...>;

template <constraint Constraint, behavior... Behaviors>
trait(Constraint, Behaviors...) -> trait<Constraint, Behaviors...>;

template <constraint Constraint1, constraint Constraint2, behavior... Behaviors>
constexpr auto operator&&(const Constraint1&, const trait<Constraint2, Behaviors...>&) noexcept {
    return trait<decltype(Constraint1{} and Constraint2{}), Behaviors...>{};
}

template <constraint Constraint1, constraint Constraint2, behavior... Behaviors>
constexpr auto operator&&(const trait<Constraint1, Behaviors...>&, const Constraint2&) noexcept {
    return trait<decltype(Constraint1{} and Constraint2{}), Behaviors...>{};
}

template <constraint Constraint1, behavior... Behaviors1, constraint Constraint2,
          behavior... Behaviors2>
constexpr auto operator&&([[maybe_unused]] const trait<Constraint1, Behaviors1...>& lhs,
                          [[maybe_unused]] const trait<Constraint2, Behaviors2...>& rhs) {
    static_assert(disjoint_from<structural_tuple<typename Behaviors1::method_type...>{},
                                structural_tuple<typename Behaviors2::method_type...>{}>,
                  "A trait can only be combined with a disjoint trait.");
    return trait<decltype(Constraint1{} and Constraint2{}), Behaviors1..., Behaviors2...>{};
}

template <constraint Constraint1, behavior... Behaviors1, constraint Constraint2,
          behavior... Behaviors2>
constexpr auto operator+(const trait<Constraint1, Behaviors1...>& lhs,
                         const trait<Constraint2, Behaviors2...>& rhs) {
    return lhs and rhs;
}

template <std::size_t I, typename Trait>
using trait_behavior_t = structural_tuple_element_t<I, Trait>;

template <std::size_t I, typename Trait>
using trait_method_t = typename trait_behavior_t<I, Trait>::method_type;

template <typename T>
struct trait_methods;

template <constraint Constraint, behavior... Behaviors>
struct trait_methods<trait<Constraint, Behaviors...>>
    : std::type_identity<structural_tuple<typename Behaviors::method_type...>> {};

template <typename T>
using trait_methods_t = typename trait_methods<T>::type;

template <typename Trait, method_name MethodName, typename T, typename... Args>
struct trait_invocable_method_index;

template <constraint Constraint, behavior... Behaviors, method_name MethodName, typename ObjectPtr,
          typename... Args>
struct trait_invocable_method_index<trait<Constraint, Behaviors...>, MethodName, ObjectPtr, Args...>
    : invocable_method_index<
          make_method_t<MethodName, std::is_const_v<std::remove_pointer_t<ObjectPtr>>, void,
                        Args...>,
          Behaviors...> {};

template <typename Trait, typename T,
          template <typename> typename PassBy = std::add_lvalue_reference>
struct trait_behaviors_table;

template <constraint Constraint, behavior... Behaviors, typename T,
          template <typename> typename PassBy>
struct trait_behaviors_table<trait<Constraint, Behaviors...>, T, PassBy> {
    using type = std::tuple<to_function_pointer_t<typename Behaviors::function_type, T, PassBy>...>;
};

template <typename Trait, typename T,
          template <typename> typename PassBy = std::add_lvalue_reference>
using trait_behaviors_table_t = typename trait_behaviors_table<Trait, T, PassBy>::type;

template <typename T, typename Trait>
struct fullfills_constraint_of;

template <typename T, constraint Constraint, behavior... Behaviors>
struct fullfills_constraint_of<T, trait<Constraint, Behaviors...>>
    : std::integral_constant<bool, Constraint{}.template operator()<T>()> {};

template <typename T, typename Trait>
struct needs_impl_for_any_behavior_of;

template <typename T, constraint Constraint, behavior... Behaviors>
struct needs_impl_for_any_behavior_of<T, trait<Constraint, Behaviors...>>
    : std::integral_constant<bool, (... or needs_impl_for<T, Behaviors>)> {};

template <typename T, typename Trait>
struct has_all_behaviors_of;

template <typename T, constraint Constraint, behavior... Behaviors>
struct has_all_behaviors_of<T, trait<Constraint, Behaviors...>>
    : std::integral_constant<bool, (... and has_behavior<T, Behaviors>)> {};

template <typename T, trait Trait>
concept has_trait = fullfills_constraint_of<std::remove_cvref_t<T>, decltype(Trait)>::value and
                    has_all_behaviors_of<std::remove_cvref_t<T>, decltype(Trait)>::value;

template <typename T, trait Trait>
concept is = has_trait<T, Trait>;
}  // namespace the_trait

inline namespace  // the_impl
{
template <behavior_implementation... Implementations>
struct impl : structural_tuple<Implementations...> {
    constexpr impl() noexcept = default;
    constexpr impl(Implementations... implementations) noexcept
        requires(sizeof...(Implementations) > 0)
        : structural_tuple<Implementations...>(implementations...) {}
    template <behavior_implementation... OtherImplementations>
    constexpr impl(impl<OtherImplementations>... implementations) noexcept
        requires std::same_as<impl<Implementations...>, impl<OtherImplementations...>>
        : structural_tuple<Implementations...>(get<OtherImplementations>(implementations)...) {}
};

template <behavior_implementation Implementation>
impl(Implementation) -> impl<Implementation>;

template <behavior_implementation... Implementations>
impl(impl<Implementations>...) -> impl<Implementations...>;

template <behavior_implementation... Implementations, behavior_implementation Additional>
constexpr auto operator,(impl<Implementations...> const& lhs,
                         impl<Additional> const& rhs) noexcept {
    return impl<Implementations..., Additional>{get<Implementations>(lhs)..., get<Additional>(rhs)};
}

template <typename T>
struct impl_methods;

template <behavior_implementation... Implementations>
struct impl_methods<impl<Implementations...>>
    : std::type_identity<structural_tuple<typename Implementations::method_type...>> {};

template <typename T>
using impl_methods_t = typename impl_methods<T>::type;

template <typename Trait, typename T>
struct implementation_for {
    template <method_id Method>
    static constexpr auto implemented() {
        using impl_type = decltype(get(std::declval<implementation_for<Trait, T>>()));
        static_assert(not std::same_as<impl_type, undefined>);

        using trait_methods_type = trait_methods_t<Trait>;
        using implemented_methods_type = impl_methods_t<impl_type>;

        static_assert(
            subset_of<implemented_methods_type{}, trait_methods_type{}>,
            "A trait implementation can only contain methods that are part of the trait.");

        // // FIXME:
        // using not_implemented_methods_type =
        // structural_tuple_without_all_from_t<trait_methods_type, implemented_methods_type>;
        // static_assert (not needs_impl_for_any_behavior_of<T,
        // not_implemented_methods_type>::value);

        return subset_of<structural_tuple{Method{}}, implemented_methods_type{}>;
    }

    template <method_id Method, typename U>
    constexpr implementation_for(method_implementation_for<Method, U>) noexcept
        requires(implemented<Method>() and std::convertible_to<U, T>)
    {}
};

template <trait Trait, typename T>
using impl_for = implementation_for<decltype(Trait), T>;
}  // namespace

inline namespace the_trait_reference {
namespace trait_reference_detail {
template <typename T, constraint Constraint, behavior... Behaviors>
    requires(not std::is_const_v<T>)
struct trait_reference_impl {
    constexpr explicit trait_reference_impl(T& object) : reference(object) {}

    template <method_name MethodName>
    constexpr auto invoke(auto&&... args) {
        constexpr auto Index =
            invocable_method_index<make_method_t<MethodName, false, void, decltype(args)...>,
                                   Behaviors...>::value;
        return invokeMethod<Index>(reference.get(), std::forward<decltype(args)>(args)...);
    }
    template <method_name MethodName>
    constexpr auto invoke(auto&&... args) const {
        constexpr auto Index =
            invocable_method_index<make_method_t<MethodName, true, void, decltype(args)...>,
                                   Behaviors...>::value;
        return invokeMethod<Index>(std::as_const(reference.get()),
                                   std::forward<decltype(args)>(args)...);
    }

   private:
    template <std::size_t Index>
        requires(Index < sizeof...(Behaviors))
    static constexpr auto invokeMethod(auto&&... args) {
        using invoke_type = typename nth_t<Index, Behaviors...>::invoke_type;
        return std::invoke(invoke_type{}, std::forward<decltype(args)>(args)...);
    }

   private:
    std::reference_wrapper<T> reference;
};
}  // namespace trait_reference_detail

template <typename T, typename Trait>
class trait_reference;

template <typename T, constraint Constraint, behavior... Behaviors>
class trait_reference<T, trait<Constraint, Behaviors...>>
    : public with_behaviors_t<
          trait_reference_detail::trait_reference_impl<T, Constraint, Behaviors...>, Behaviors...> {
    using base_type =
        with_behaviors_t<trait_reference_detail::trait_reference_impl<T, Constraint, Behaviors...>,
                         Behaviors...>;
    using base_type::invoke;

   public:
    constexpr explicit trait_reference(T& object) : base_type(object) {}
};

template <typename T, constraint Constraint, behavior... Behaviors>
class trait_reference<const T, trait<Constraint, Behaviors...>>
    : public with_immutable_behaviors_t<
          trait_reference_detail::trait_reference_impl<T, Constraint, Behaviors...>, Behaviors...> {
    using base_type = with_immutable_behaviors_t<
        trait_reference_detail::trait_reference_impl<T, Constraint, Behaviors...>, Behaviors...>;
    using base_type::invoke;

   public:
    constexpr explicit trait_reference(T const& object) : base_type(const_cast<T&>(object)) {}
};

template <trait Trait>
constexpr auto trait_cast(is<Trait> auto& object) {
    return trait_reference<std::remove_reference_t<decltype(object)>, decltype(Trait)>{object};
}

template <trait Trait>
constexpr auto as(is<Trait> auto& object) {
    return trait_cast<Trait>(object);
}
}  // namespace the_trait_reference

inline namespace methods {
template <auto MethodName>
concept method_name_was_registered = false;

template <method_name MethodName>
    requires method_name_was_registered<MethodName>
struct undefined_method {};

template <method_name MethodName>
struct incomplete_method {
    constexpr auto operator=(callable auto&& implementation) const noexcept {
        using implementation_type = std::remove_cvref_t<decltype(implementation)>;
        using function_type = function_type_from_implementation_t<implementation_type>;
        using method_type = method_signature<MethodName, function_type>;

        return impl{method_implementation<method_type, implementation_type>{
            std::forward<decltype(implementation)>(implementation)}};
    }
};

template <method_name MethodName, function_type... FunctionSignatures>
    requires(sizeof...(FunctionSignatures) <= 1)
constexpr undefined_method<MethodName> Method;

template <method_name MethodName>
constexpr incomplete_method<MethodName> Method<MethodName>{};

template <method_name MethodName>
constexpr auto operator""_method() noexcept {
    return Method<MethodName>;
}
}  // namespace methods
}  // namespace traits

#define traits_method_adder_lambda_(name, lambda_name, using_name, const_suffix)                  \
    constexpr auto lambda_name = []<function_type S, typename Type, typename InvokeMethod>() {    \
        class with_arity_0 : public Type {                                                        \
           public:                                                                                \
            using_name auto name() const_suffix { return std::invoke(InvokeMethod{}, this); }     \
                                                                                                  \
           protected:                                                                             \
            using Type::Type;                                                                     \
        };                                                                                        \
        class with_arity_1 : public Type {                                                        \
           public:                                                                                \
            using_name auto name(nth_argument_t<0, S> a0) const_suffix {                          \
                return std::invoke(InvokeMethod{}, this, ::traits::forward_arg<0, S>(a0));        \
            }                                                                                     \
                                                                                                  \
           protected:                                                                             \
            using Type::Type;                                                                     \
        };                                                                                        \
        class with_arity_2 : public Type {                                                        \
           public:                                                                                \
            using_name auto name(nth_argument_t<0, S> a0, nth_argument_t<1, S> a1) const_suffix { \
                return std::invoke(InvokeMethod{}, this, ::traits::forward_arg<0, S>(a0),         \
                                   ::traits::forward_arg<1, S>(a1));                              \
            }                                                                                     \
                                                                                                  \
           protected:                                                                             \
            using Type::Type;                                                                     \
        };                                                                                        \
        class with_arity_3 : public Type {                                                        \
           public:                                                                                \
            using_name auto name(nth_argument_t<0, S> a0, nth_argument_t<1, S> a1,                \
                                 nth_argument_t<2, S> a2) const_suffix {                          \
                return std::invoke(InvokeMethod{}, this, ::traits::forward_arg<0, S>(a0),         \
                                   ::traits::forward_arg<1, S>(a1),                               \
                                   ::traits::forward_arg<2, S>(a2));                              \
            }                                                                                     \
                                                                                                  \
           protected:                                                                             \
            using Type::Type;                                                                     \
        };                                                                                        \
        class with_arity_4 : public Type {                                                        \
           public:                                                                                \
            using_name auto name(nth_argument_t<0, S> a0, nth_argument_t<1, S> a1,                \
                                 nth_argument_t<2, S> a2, nth_argument_t<3, S> a3) const_suffix { \
                return std::invoke(InvokeMethod{}, this, ::traits::forward_arg<0, S>(a0),         \
                                   ::traits::forward_arg<1, S>(a1),                               \
                                   ::traits::forward_arg<2, S>(a2),                               \
                                   ::traits::forward_arg<3, S>(a3));                              \
            }                                                                                     \
                                                                                                  \
           protected:                                                                             \
            using Type::Type;                                                                     \
        };                                                                                        \
        class with_arity_5 : public Type {                                                        \
           public:                                                                                \
            using_name auto name(nth_argument_t<0, S> a0, nth_argument_t<1, S> a1,                \
                                 nth_argument_t<2, S> a2, nth_argument_t<3, S> a3,                \
                                 nth_argument_t<4, S> a4) const_suffix {                          \
                return std::invoke(                                                               \
                    InvokeMethod{}, this, ::traits::forward_arg<0, S>(a0),                        \
                    ::traits::forward_arg<1, S>(a1), ::traits::forward_arg<2, S>(a2),             \
                    ::traits::forward_arg<3, S>(a3), ::traits::forward_arg<4, S>(a4));            \
            }                                                                                     \
                                                                                                  \
           protected:                                                                             \
            using Type::Type;                                                                     \
        };                                                                                        \
        return std::type_identity<                                                                \
            ::traits::nth_t<::traits::arity<S>::value, with_arity_0, with_arity_1, with_arity_2,  \
                            with_arity_3, with_arity_4, with_arity_5>>{};                         \
    }

#define traits_method_adder_(name)                                                                \
    []<function_type Signature_, typename Overloads_, typename MaybeConstType,                    \
       typename InvokeMethod_>() {                                                                \
        traits_method_adder_lambda_(name, unique_method, /* using Type :: name; */, /* const*/);  \
        traits_method_adder_lambda_(name, unique_const_method, /* using Type :: name; */, const); \
        traits_method_adder_lambda_(name, overloaded_method, using Type ::name;, /* const*/);     \
        traits_method_adder_lambda_(name, overloaded_const_method, using Type ::name;, const);    \
                                                                                                  \
        using Type_ = std::remove_const_t<MaybeConstType>;                                        \
        if constexpr (std::is_const_v<MaybeConstType> and                                         \
                      not ::traits::constness<Signature_>::value)                                 \
            return std::type_identity<Type_>{};                                                   \
        else if constexpr (Overloads_::value)                                                     \
            if constexpr (::traits::constness<Signature_>::value)                                 \
                return ::traits::generic_lambda_result_t<overloaded_const_method, Signature_,     \
                                                         Type_, InvokeMethod_>{};                 \
            else                                                                                  \
                return ::traits::generic_lambda_result_t<overloaded_method, Signature_, Type_,    \
                                                         InvokeMethod_>{};                        \
        else if constexpr (::traits::constness<Signature_>::value)                                \
            return ::traits::generic_lambda_result_t<unique_const_method, Signature_, Type_,      \
                                                     InvokeMethod_>{};                            \
        else                                                                                      \
            return ::traits::generic_lambda_result_t<unique_method, Signature_, Type_,            \
                                                     InvokeMethod_>{};                            \
    }

#define traits_can_invoke_(name)                                \
    []<auto command, typename Object, typename... Args>() {     \
        return requires(Object object, Args... args) {          \
            object.name(std::forward<decltype(args)>(args)...); \
        };                                                      \
    }

#define traits_invoke_(name) \
    [](auto& object, auto&&... args) { return object.name(std::forward<decltype(args)>(args)...); }

#define traits_method_kernel_(name)                                                \
    ::traits::overload {                                                           \
        traits_method_adder_(name), traits_can_invoke_(name), traits_invoke_(name) \
    }
#define traits_method_signature_(name, parameters) \
    ::traits::method_signature<#name, ::traits::canonical_function_t<parameters>>

#define TRAITS_METHOD(name, parameters) \
    ::traits::method<traits_method_signature_(name, parameters), traits_method_kernel_(name)> {}

#define TRAITS_METHOD_DECLARATION(name)             \
    template <::traits::function_type FunctionType> \
    constexpr auto ::traits::Method<#name, FunctionType> = TRAITS_METHOD(name, FunctionType)

TRAITS_METHOD_DECLARATION(operator());

// some

namespace traits {
namespace detail {
constexpr auto is_multiple_of(std::integral auto maybe_multiple, std::integral auto value) -> bool {
    return std::lcm(maybe_multiple, value) == maybe_multiple;
}

constexpr auto is_power_of_two(std::integral auto value) -> bool {
    return std::has_single_bit(value);
}
}  // namespace detail

constexpr std::size_t PointerSize = sizeof(void*);
constexpr std::size_t DefaultAlignment = PointerSize;

template <std::size_t Size = PointerSize, std::size_t Alignment = DefaultAlignment>
    requires(Alignment >= PointerSize and detail::is_power_of_two(Alignment) and
             detail::is_multiple_of(Size, Alignment))
struct alignas(Alignment) buffer {
    constexpr auto memory() -> void* { return data; }
    [[nodiscard]] constexpr auto memory() const -> const void* { return data; }

    char data[Size];
};

template <typename T>
concept memory = requires(T& t) {
    { t.memory() } -> std::convertible_to<void*>;
    { std::as_const(t).memory() } -> std::convertible_to<const void*>;
};

template <constraint OtherConstraints = unconstrained_type, typename... Alternatives>
    requires(sizeof...(Alternatives) > 1)
struct variant_constraint {
    template <std::size_t Index>
    using alternative_type = nth_t<Index, Alternatives...>;

    static constexpr std::integral_constant<std::size_t, sizeof...(Alternatives)>
        AlternativeCount{};

    template <typename T>
    constexpr auto operator()() const noexcept {
        return (... or std::same_as<T, Alternatives>) and
               OtherConstraints{}.template operator()<T>();
    }
};

template <typename T>
struct is_variant_constraint : std::false_type {};

template <typename... Types>
struct is_variant_constraint<variant_constraint<Types...>> : std::true_type {};

template <constraint OtherConstraints, typename... Types, constraint Constraint>
constexpr auto operator&&(const variant_constraint<OtherConstraints, Types...>&,
                          const Constraint&) noexcept {
    static_assert(not is_variant_constraint<Constraint>::value);
    return variant_constraint<decltype(OtherConstraints{} and Constraint{}), Types...>{};
}

template <constraint Constraint, constraint OtherConstraints, typename... Types>
constexpr auto operator&&(const Constraint&,
                          const variant_constraint<OtherConstraints, Types...>&) noexcept {
    static_assert(not is_variant_constraint<Constraint>::value);
    return variant_constraint<decltype(Constraint{} and OtherConstraints{}), Types...>{};
}

class bad_some_cast : public std::bad_cast {
   public:
    [[nodiscard]] auto what() const noexcept -> const char* override { return "bad some_cast"; }
};

template <typename Trait, memory InlineBuffer, typename InlinedMethods>
class some_impl {
    using trait_type = Trait;
    using constraint_type = typename trait_type::constraint_type;

   protected:
    explicit some_impl(auto&& object)
        requires(not std::derived_from<std::remove_cvref_t<decltype(object)>, some_impl>)
    {
        using object_type = std::remove_cvref_t<decltype(object)>;

        static_assert(has_trait<object_type, trait_type{}>);
        static_assert(std::copyable<object_type>);

        if constexpr (inlined_storage<object_type>::value)
            new (storage.inlined.memory()) object_type{std::forward<decltype(object)>(object)};
        else
            storage.dynamic = new object_type{std::forward<decltype(object)>(object)};

        this->methods = methods_for_type<object_type>();
        this->inlined = behaviors_table_for<InlinedMethods, object_type>();
    }

   public:
    some_impl(const some_impl& other) {
        this->methods = other.methods;

        if (this->methods) {
            this->methods->copy(other.storage, this->storage);
            this->inlined = other.inlined;
        }
    }
    some_impl(some_impl&& other) noexcept {
        this->methods = std::exchange(other.methods, nullptr);

        if (this->methods) {
            this->methods->move(other.storage, this->storage);
            this->inlined = std::move(other.inlined);
        }
    }
    auto operator=(const some_impl& other) noexcept -> some_impl& {
        some_impl{other}.swap(*this);
        return *this;
    }
    auto operator=(some_impl&& other) noexcept -> some_impl& {
        some_impl{std::move(other)}.swap(*this);
        return *this;
    }
    ~some_impl() { reset(); }

    void reset() noexcept {
        if (auto oldMethods = std::exchange(this->methods, nullptr); oldMethods) {
            oldMethods->destroy(storage);
        }
    }

    [[nodiscard]] auto has_value() const noexcept -> bool { return this->methods != nullptr; }

    [[nodiscard]] auto type() const noexcept -> const std::type_info& {
        return has_value() ? this->methods->type() : typeid(void);
    }

    void swap(some_impl& other) noexcept {
        if (this->methods != other.methods) {
            some_impl tmp{std::move(other)};

            other.methods = this->methods;
            if (other.methods != nullptr) {
                other.methods->move(this->storage, other.storage);
                other.inlined = std::move(this->inlined);
            }

            this->methods = std::exchange(tmp.methods, nullptr);
            if (this->methods != nullptr) {
                this->methods->move(tmp.storage, this->storage);
                this->inlined = std::move(tmp.inlined);
            }
        } else if (this->methods != nullptr) {
            this->methods->swap(this->storage, other.storage);
        }
    }

    template <method_name MethodName>
    auto invoke(auto&&... args) {
        constexpr auto Index = trait_invocable_method_index<trait_type, MethodName, decltype(this),
                                                            decltype(args)...>::value;
        constexpr auto InlineMethodIndex =
            trait_invocable_method_index<InlinedMethods, MethodName, decltype(this),
                                         decltype(args)...>::value;

        if (not has_value()) throw std::bad_function_call{};

        return this->invokeMethod<Index, InlineMethodIndex>(std::forward<decltype(args)>(args)...);
    }
    template <method_name MethodName>
    auto invoke(auto&&... args) const {
        constexpr auto Index = trait_invocable_method_index<trait_type, MethodName, decltype(this),
                                                            decltype(args)...>::value;
        constexpr auto InlineMethodIndex =
            trait_invocable_method_index<InlinedMethods, MethodName, decltype(this),
                                         decltype(args)...>::value;

        if (not has_value()) throw std::bad_function_call{};

        return this->invokeMethod<Index, InlineMethodIndex>(std::forward<decltype(args)>(args)...);
    }

    friend constexpr auto visit(auto&& visitor, auto&& self)
        requires(std::derived_from<std::remove_cvref_t<decltype(self)>, some_impl> and
                 is_variant_constraint<constraint_type>::value)
    {
        auto doVisit = [&]<std::size_t Index = 0>(auto doVisit) {
            if constexpr (Index + 1 < constraint_type::AlternativeCount()) {
                using T = constraint_type::template alternative_type<Index>;

                if (typeid(T) == self.type())
                    return visitor(some_cast<T>(std::forward<decltype(self)>(self)));
                else
                    return doVisit.template operator()<Index + 1>(doVisit);
            } else {
                using T = constraint_type::template alternative_type<Index>;
                assert(typeid(T) == self.type());

                return visitor(some_cast<T>(std::forward<decltype(self)>(self)));
            }
        };

        if (not self.has_value()) throw std::bad_variant_access{};

        return doVisit(doVisit);
    }

   protected:
    friend void swap(some_impl& lhs, some_impl& rhs) noexcept { lhs.swap(rhs); }

    template <typename T>
    friend auto some_cast(const some_impl& operand) -> T {
        using U = std::remove_cvref_t<T>;

        auto ptr = some_cast<U>(&operand);
        if (ptr == nullptr) throw bad_some_cast{};

        return static_cast<T>(*ptr);
    }

    template <typename T>
    friend auto some_cast(some_impl& operand) -> T {
        using U = std::remove_cvref_t<T>;

        auto ptr = some_cast<U>(&operand);
        if (ptr == nullptr) throw bad_some_cast{};

        return static_cast<T>(*ptr);
    }

    template <typename T>
    friend auto some_cast(some_impl&& operand) -> T {
        using U = std::remove_cvref_t<T>;

        auto ptr = some_cast<U>(&operand);
        if (ptr == nullptr) throw bad_some_cast{};

        return static_cast<T>(std::move(*ptr));
    }

    template <typename T>
    friend auto some_cast(const some_impl* operand) noexcept -> const T* {
        if (operand == nullptr or operand->type() != typeid(T))
            return nullptr;
        else
            return &cast<T>(operand->storage);
    }

    template <typename T>
    friend auto some_cast(some_impl* operand) noexcept -> T* {
        if (operand == nullptr or operand->type() != typeid(T))
            return nullptr;
        else
            return &cast<T>(operand->storage);
    }

   private:
    template <std::size_t Index, std::size_t InlineMethodIndex>
    static constexpr auto canInvokeInlineMethod() -> bool {
        if constexpr (InlineMethodIndex < InlinedMethods::size())
            return std::same_as<trait_method_t<Index, trait_type>,
                                trait_method_t<InlineMethodIndex, InlinedMethods>>;
        else
            return false;
    }

    template <std::size_t Index, std::size_t InlineMethodIndex>
        requires(Index < trait_type::size())
    auto invokeMethod(auto&&... args) {
        if constexpr (canInvokeInlineMethod<Index, InlineMethodIndex>())
            return std::invoke(std::get<InlineMethodIndex>(this->inlined), this->storage,
                               std::forward<decltype(args)>(args)...);
        else
            return std::invoke(std::get<Index>(this->methods->behaviors), this->storage,
                               std::forward<decltype(args)>(args)...);
    }
    template <std::size_t Index, std::size_t InlineMethodIndex>
        requires(Index < trait_type::size())
    auto invokeMethod(auto&&... args) const {
        if constexpr (canInvokeInlineMethod<Index, InlineMethodIndex>())
            return std::invoke(std::get<InlineMethodIndex>(this->inlined), this->storage,
                               std::forward<decltype(args)>(args)...);
        else
            return std::invoke(std::get<Index>(this->methods->behaviors), this->storage,
                               std::forward<decltype(args)>(args)...);
    }

   private:
    union storage_union {
        InlineBuffer inlined;
        void* dynamic;
    };

    struct methods_type {
        const std::type_info& (*type)() noexcept;
        void (*destroy)(storage_union&) noexcept;
        void (*copy)(const storage_union& src, storage_union& dest);
        void (*move)(storage_union& src, storage_union& dest) noexcept;
        void (*swap)(storage_union& lhs, storage_union& rhs) noexcept;

        trait_behaviors_table_t<trait_type, storage_union> behaviors;
    };

    template <typename T>
    struct common_methods {
        static auto type() noexcept -> const std::type_info& { return typeid(T); }
    };

    template <typename T>
    struct dynamic_storage_methods : common_methods<T> {
        static void destroy(storage_union& storage) noexcept {
            delete static_cast<T*>(storage.dynamic);
        }

        static void copy(const storage_union& src, storage_union& dest) {
            dest.dynamic = new T{*static_cast<const T*>(src.dynamic)};
        }

        static void move(storage_union& src, storage_union& dest) noexcept {
            dest.dynamic = std::exchange(src.dynamic, nullptr);
        }

        static void swap(storage_union& lhs, storage_union& rhs) noexcept {
            std::swap(lhs.dynamic, rhs.dynamic);
        }
    };

    template <typename T>
    struct inlined_storage_methods : common_methods<T> {
        static void destroy(storage_union& storage) noexcept {
            static_cast<T*>(storage.inlined.memory())->~T();
        }

        static void copy(const storage_union& src, storage_union& dest) {
            new (dest.inlined.memory()) T{*static_cast<const T*>(src.inlined.memory())};
        }

        static void move(storage_union& src, storage_union& dest) noexcept {
            new (dest.inlined.memory()) T{std::move(*static_cast<T*>(src.inlined.memory()))};
            destroy(src);
        }

        static void swap(storage_union& lhs, storage_union& rhs) noexcept {
            storage_union tmp;
            move(rhs, tmp);
            move(lhs, rhs);
            move(tmp, lhs);
        }
    };

    template <typename T>
    struct inlined_storage
        : std::integral_constant<bool, std::is_nothrow_move_constructible_v<T> and
                                           sizeof(T) <= sizeof(InlineBuffer) and
                                           std::alignment_of_v<T> <=
                                               std::alignment_of_v<InlineBuffer>> {};

    template <typename T>
    static constexpr auto cast(const storage_union& storage) noexcept -> const T& {
        if constexpr (inlined_storage<T>::value)
            return *static_cast<const T*>(storage.inlined.memory());
        else
            return *static_cast<const T*>(storage.dynamic);
    }
    template <typename T>
    static constexpr auto cast(storage_union& storage) noexcept -> T& {
        if constexpr (inlined_storage<T>::value)
            return *static_cast<T*>(storage.inlined.memory());
        else
            return *static_cast<T*>(storage.dynamic);
    }

    template <behavior Behavior, typename T>
    static constexpr auto behavior_for() {
        using Impl = decltype([](auto& storage, auto&&... args) {
            using invoke_type = typename Behavior::invoke_type;
            return std::invoke(invoke_type{}, cast<T>(storage),
                               std::forward<decltype(args)>(args)...);
        });

        return make_callable_t<storage_union, typename Behavior::function_type, Impl>{};
    }

    template <typename table_trait_type, typename T, std::size_t... Is>
    static constexpr auto behaviors_table_for(std::index_sequence<Is...>)
        -> trait_behaviors_table_t<table_trait_type, storage_union> {
        return {behavior_for<trait_behavior_t<Is, table_trait_type>, T>()...};
    }

    template <typename table_trait_type, typename T>
    static constexpr auto behaviors_table_for()
        -> trait_behaviors_table_t<table_trait_type, storage_union> {
        return behaviors_table_for<table_trait_type, T>(
            std::make_index_sequence<table_trait_type::size()>{});
    }

    template <typename T>
    static auto methods_for_type() -> methods_type* {
        using VTableType = std::conditional_t<inlined_storage<T>::value, inlined_storage_methods<T>,
                                              dynamic_storage_methods<T>>;

        static constinit methods_type table{VTableType::type,
                                            VTableType::destroy,
                                            VTableType::copy,
                                            VTableType::move,
                                            VTableType::swap,

                                            behaviors_table_for<trait_type, T>()};

        return &table;
    }

   private:
    storage_union storage;
    methods_type* methods;
    [[no_unique_address]] trait_behaviors_table_t<InlinedMethods, storage_union> inlined;
};

// FIXME:

// inline speichert einen matcher
// das trait wird später mit dem matcher gefiltert

// es gibt einen matcher für alles, einen matcher für gar nichts und einen matcher für eine methode
// der matcher für einen Methode kann mit einer incomplete_method oder einer method initialisiert
// werden

template <behavior... Behaviors>
struct inlined {
    // as_trait => behaviors
    trait<unconstrained_type, Behaviors...> as_trait;
};

template <behavior... Behaviors>
inlined(Behaviors...) -> inlined<Behaviors...>;

template <behavior... Behaviors>
inlined(trait<unconstrained_type, Behaviors...>) -> inlined<Behaviors...>;

[[maybe_unused]] constexpr inline auto NoInlinedMethods = inlined{};

template <typename Trait, memory InlineBuffer, trait InlinedMethods>
struct some_with_behaviors;

template <constraint Constraint, behavior... Behaviors, memory InlineBuffer, trait InlinedMethods>
struct some_with_behaviors<trait<Constraint, Behaviors...>, InlineBuffer, InlinedMethods> {
    using type = with_behaviors_t<
        some_impl<trait<Constraint, Behaviors...>, InlineBuffer, decltype(InlinedMethods)>,
        Behaviors...>;
};

template <typename Trait, memory InlineBuffer, trait InlinedMethods>
using some_with_behaviors_t =
    typename some_with_behaviors<Trait, InlineBuffer, InlinedMethods>::type;

template <typename U>
constexpr auto SameAs = []<typename T>() { return std::same_as<T, U>; };

template <trait Trait = trait{}, memory InlineBuffer = buffer<>,
          inlined InlinedMethods = NoInlinedMethods>
// FIXME: requires subset_of<InlinedMethods.as_trait, Trait>
class some final
    : public some_with_behaviors_t<decltype(Trait), InlineBuffer, InlinedMethods.as_trait> {
    using base_type = some_with_behaviors_t<decltype(Trait), InlineBuffer, InlinedMethods.as_trait>;
    using base_type::invoke;

    template <typename U>
    static constexpr auto SameAs = []<typename T>() { return std::same_as<T, U>; };

   public:
    some(is<Trait and not SameAs<some>> auto&& value)
        : base_type(std::forward<decltype(value)>(value)) {
        using value_type = std::remove_cvref_t<decltype(value)>;
        static_assert(std::copyable<value_type>);
    }

    template <typename... Alternatives>
    using variant = some<variant_constraint<unconstrained_type, Alternatives...>{} and Trait,
                         InlineBuffer, InlinedMethods>;
};

template <typename... Alternatives>
constexpr auto common_alignment() -> std::size_t {
    std::size_t alignment = 1;
    return (..., (alignment = std::lcm(alignment, std::alignment_of_v<Alternatives>)));
}

constexpr auto next_multiple_of(std::size_t value, std::size_t multiple) -> std::size_t {
    return ((value + multiple - 1) / multiple) * multiple;
}

template <typename... Alternatives>
constexpr auto common_size() -> std::size_t {
    std::size_t size = 1;
    return next_multiple_of((..., (size = std::max(size, sizeof(Alternatives)))),
                            common_alignment<Alternatives...>());
}

template <typename... Alternatives>
using some_variant_buffer_t =
    buffer<common_size<Alternatives...>(), common_alignment<Alternatives...>()>;

template <typename... Alternatives>
using some_variant =
    some<Unconstrained, some_variant_buffer_t<Alternatives...>>::template variant<Alternatives...>;
}  // namespace traits

#endif  // TRAITS_H_
