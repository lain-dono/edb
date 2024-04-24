use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use syn::{Attribute, DataStruct, Field, Fields, FieldsNamed, Type};

#[proc_macro_derive(Table, attributes(index, index_set))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = syn::parse_macro_input!(input as syn::DeriveInput);
    let gen: TokenStream = impl_from_args(&ast);
    gen.into()
}

fn impl_from_args(input: &syn::DeriveInput) -> TokenStream {
    match &input.data {
        syn::Data::Struct(DataStruct {
            fields: Fields::Named(FieldsNamed { named: fields, .. }),
            ..
        }) => {
            let vis = &input.vis;
            let name = &input.ident;
            let table = format_ident!("{}Table", input.ident);
            let key = format_ident!("{}Key", input.ident);

            let gen: Vec<_> = fields.iter().filter_map(GenField::parse).collect();

            let indexes = gen.iter().map(|f| f.fields(&key));
            let getters = gen.iter().map(|f| f.find(name, &key));
            let removers = gen.iter().map(|f| f.remove());
            let inserters = gen.iter().map(|f| f.insert());

            let clear = gen.iter().map(|f| f.clear());
            let with_capacity = gen.iter().map(|f| f.with_capacity());

            quote! {
                ::edb::slotmap::new_key_type! {
                    #vis struct #key;
                }

                #[derive(::core::default::Default)]
                #vis struct #table {
                    storage: ::edb::Storage<#key, #name>,
                    #( #indexes )*
                }

                impl ::core::ops::Index<#key> for #table {
                    type Output = #name;

                    fn index(&self, key: #key) -> &Self::Output {
                        &self.storage[key]
                    }
                }

                impl ::core::ops::IndexMut<#key> for #table {
                    fn index_mut(&mut self, key: #key) -> &mut Self::Output {
                        &mut self.storage[key]
                    }
                }

                impl #table {
                    pub fn get(&self, key: #key) -> ::core::option::Option<&#name> {
                        self.storage.get(key)
                    }

                    pub fn get_mut(&mut self, key: #key) -> ::core::option::Option<&mut #name> {
                        self.storage.get_mut(key)
                    }

                    pub fn insert(&mut self, value: #name) -> #key {
                        self.storage.insert_with_key(|key| {
                            #( #inserters )*
                            value
                        })
                    }

                    pub fn remove(&mut self, key: #key) -> ::core::option::Option<#name> {
                        self.storage.remove(key).map(|value| {
                            #( #removers )*
                            value
                        })
                    }

                    pub fn clear(&mut self) {
                        self.storage.clear();
                        #( #clear )*
                    }

                    pub fn with_capacity(capacity: usize) -> Self {
                        Self {
                            storage: ::::edb::Storage::with_capacity_and_key(capacity),

                            #( #with_capacity )*
                        }
                    }

                    #( #getters )*
                }

            }
        }
        _ => TokenStream::new(),
    }
}

fn has_ident(attrs: &[Attribute], ident: &str) -> bool {
    attrs
        .iter()
        .filter(|attr| matches!(attr.style, syn::AttrStyle::Outer))
        .any(|attr| attr.path().is_ident(ident))
}

struct GenField {
    name: Ident,
    ty: Type,
    index: GenIndex,
}

enum GenIndex {
    Index,
    IndexSet,
}

impl GenField {
    fn new(name: Ident, ty: Type, index: GenIndex) -> Self {
        Self { name, ty, index }
    }

    fn parse(
        Field {
            ident, ty, attrs, ..
        }: &Field,
    ) -> Option<Self> {
        let name = ident.as_ref()?.clone();

        if has_ident(attrs, "index") {
            let field = GenField::new(name, ty.clone(), GenIndex::Index);
            return Some(field);
        }

        if has_ident(attrs, "index_set") {
            let field = GenField::new(name, ty.clone(), GenIndex::IndexSet);
            return Some(field);
        }

        None
    }

    fn fields(&self, key: &Ident) -> TokenStream {
        let index = format_ident!("index_{}", self.name);
        let ty = &self.ty;
        match self.index {
            GenIndex::Index => quote! { #index : ::edb::Index<#ty, #key>, },
            GenIndex::IndexSet => quote! { #index : ::edb::IndexSet<#ty, #key>, },
        }
    }

    fn with_capacity(&self) -> TokenStream {
        let index = format_ident!("index_{}", self.name);
        match self.index {
            GenIndex::Index => quote! { #index : ::edb::Index::with_capacity(capacity), },
            GenIndex::IndexSet => quote! { #index : ::edb::IndexSet::with_capacity(capacity), },
        }
    }

    fn clear(&self) -> TokenStream {
        let index = format_ident!("index_{}", self.name);
        match self.index {
            GenIndex::Index => quote! { self.#index.clear(); },
            GenIndex::IndexSet => quote! { self.#index.clear(); },
        }
    }

    fn insert(&self) -> TokenStream {
        let name = &self.name;
        let index = format_ident!("index_{}", self.name);

        match self.index {
            GenIndex::Index => quote! {
                self.#index.insert(value.#name.clone(), key);
            },
            GenIndex::IndexSet => quote! {
                self.#index.entry(value.#name.clone()).or_default().insert(key);
            },
        }
    }

    fn remove(&self) -> TokenStream {
        let name = &self.name;
        let index = format_ident!("index_{}", self.name);

        match self.index {
            GenIndex::Index => quote! {
                self.#index.remove(&value.#name);
            },
            GenIndex::IndexSet => quote! {
                if let ::core::option::Option::Some(set) = self.#index.get_mut(&value.#name) {
                    set.remove(&key);
                    if set.is_empty() {
                        self.#index.remove(&value.#name);
                    }
                };
            },
        }
    }

    fn find(&self, value: &Ident, key: &Ident) -> TokenStream {
        let name = &self.name;
        let ty = &self.ty;
        let index = format_ident!("index_{}", self.name);

        match self.index {
            GenIndex::Index => {
                let find_by = format_ident!("find_by_{}", self.name);
                let key_by = format_ident!("key_by_{}", self.name);
                let find_by_mut = format_ident!("find_by_{}_mut", self.name);
                let remove_by = format_ident!("remove_by_{}", self.name);

                quote! {
                    pub fn #find_by<Q>(&self, #name: &Q) -> ::core::option::Option<&#value>
                    where
                        #ty: ::core::borrow::Borrow<Q>,
                        Q: ::core::hash::Hash + ::core::cmp::Eq + ?::core::marker::Sized,
                    {
                        self.#index.get(#name).map(|&key| &self.storage[key])
                    }

                    pub fn #key_by<Q>(&self, #name: &Q) -> ::core::option::Option<#key>
                    where
                        #ty: ::core::borrow::Borrow<Q>,
                        Q: ::core::hash::Hash + ::core::cmp::Eq + ?::core::marker::Sized,
                    {
                        self.#index.get(#name).copied()
                    }

                    pub fn #find_by_mut<Q>(&mut self, #name: &Q) -> ::core::option::Option<&mut #value>
                    where
                        #ty: ::core::borrow::Borrow<Q>,
                        Q: ::core::hash::Hash + ::core::cmp::Eq + ?::core::marker::Sized,
                    {
                        self.#index.get(#name).map(|&key| &mut self.storage[key])
                    }

                    pub fn #remove_by<Q>(&mut self, #name: &Q) -> ::core::option::Option<#value>
                    where
                        #ty: ::core::borrow::Borrow<Q>,
                        Q: ::core::hash::Hash + ::core::cmp::Eq + ?::core::marker::Sized,
                    {
                        self.#index.get(#name).copied().and_then(|key| self.remove(key))
                    }
                }
            }
            GenIndex::IndexSet => {
                let contains = format_ident!("contains_{}", self.name);
                let list_by = format_ident!("list_by_{}", self.name);
                let keys_by = format_ident!("keys_by_{}", self.name);

                quote! {
                    pub fn #contains<Q>(&self, #name: &Q) -> bool
                    where
                        #ty: ::core::borrow::Borrow<Q>,
                        Q: ::core::hash::Hash + ::core::cmp::Eq + ?::core::marker::Sized,
                    {
                        self.#index.contains_key(#name)
                    }

                    pub fn #list_by<Q>(&self, #name: &Q) -> ::core::option::Option<impl ::core::iter::Iterator<Item = &#value>>
                    where
                        #ty: ::core::borrow::Borrow<Q>,
                        Q: ::core::hash::Hash + ::core::cmp::Eq + ?::core::marker::Sized,
                    {
                        self.#index.get(#name).map(|keys| keys.iter().map(|&key| &self.storage[key]))
                    }

                    pub fn #keys_by<Q>(&self, #name: &Q) -> ::core::option::Option<impl ::core::iter::Iterator<Item = #key> + '_>
                    where
                        #ty: ::core::borrow::Borrow<Q>,
                        Q: ::core::hash::Hash + ::core::cmp::Eq + ?::core::marker::Sized,
                    {
                        self.#index.get(#name).map(|keys| keys.iter().copied())
                    }
                }
            }
        }
    }
}
