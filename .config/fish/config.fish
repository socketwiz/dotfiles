if status is-interactive
    # Commands to run in interactive sessions can go here
    neofetch
    starship init fish | source

    # load fzf bindings
    source /usr/share/doc/fzf/examples/key-bindings.fish
    fzf_key_bindings
end
