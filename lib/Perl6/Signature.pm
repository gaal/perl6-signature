package Perl6::Signature;

use warnings;

use Parse::RecDescent;
use Text::Balanced;
use Perl6::Signature::Val;

#$::RD_TRACE = 1;
$::RD_HINT  = 1;

our $SIGNATURE_GRAMMAR = << '.';
    { use Text::Balanced qw(extract_bracketed); }

    Sig: Sig_colon | Sig_nocolon

    Sig_colon: ':' Sig_nocolon

    Sig_nocolon: '(' Sigbody ')'
        { $item{Sigbody} }

    Sigbody: Sigbody_inv | Sigbody_noinv

    Sigbody_inv: Invocant <skip:'\s*'> ':' Sigbody_noinv
        {
          my $sig = $item{Sigbody_noinv};
          die "invocant cannot be optional" unless $item{Invocant}->{required};
          $sig->s_invocant( $item{Invocant}->{param} );
          $return = $sig;
        }

    Sigbody_noinv: Param(s? /,/)
        {
          # calculate requiredPositionalCount, and make sure we don't have
          # :($optional?, $required!) -- that's invalid.

          my $params = $item{'Param(s?)'};
          my @positionals = grep { $_->{style} eq 'positional' } @$params;
          my @named = grep { $_->{style} eq 'named' } @$params;
          my $seen_optional;
          my $requiredPositionalCount = 0;
          for my $param (@positionals) {
              $seen_optional++ if ! $param->{required};
              die "can't place required positional after an optional one" if
                  $param->{required} && $seen_optional;
              $requiredPositionalCount++ if ! $seen_optional;
          }
          my $sig = Perl6::Signature::Val::Sig->new
              ( s_requiredPositionalCount => $requiredPositionalCount
              , s_positionalList          => [ map { $_->{param} } @positionals ]
              , s_namedList               => [ map { $_->{param} } @named ]
              , s_requiredNames           => { map { $_->{param}->p_label => 1 } grep { $_->{required} } @named }
              );
          $return = $sig;
        }

    Invocant: Param

    Param: ParamType(s?)
           ParamIdentifier <skip:''>
           OptionalityModifier(?)
           <skip:'\s*'>
           Unpacking(?)
           DefaultValueSpec(?)
           <skip:'\s+'>
           Attrib(s?)
           <skip:'\s*'>
           Constraint(s?)
        {
          my ($variable, $label, $style) =
                @{$item{ParamIdentifier}}{qw/variable label style/};

          my ($hasAccess, $isRef, $isContext, $isLazy, @slots);
          # unfortunately, we can't use a hash and delete from it:
          # "is ro is rw" means "is rw". (Or maybe, a parse error.)
          ATTR: for (@{ $item{'Attrib(s?)'} }) {
            /^(ro|rw|copy)$/ && do { $hasAccess = $_; next ATTR };
            /^ref$/          && do { $isRef = 1;      next ATTR };
            /^context$/      && do { $isContext = 1;  next ATTR };
            /^lazy$/         && do { $isLazy = 1;     next ATTR };
            push @slots, $_;
          }

          my $param = Perl6::Signature::Val::SigParam->new
                ( p_types     => $item{'ParamType(s?)'}
                , p_variable  => $variable
                , p_label     => $label
                , ($item{'Unpacking(?)'} ? (p_unpacking => $item{'Unpacking(?)'}->[0]) : ())
                , (@{ $item{'DefaultValueSpec(?)'} } ?
                        (p_default => $item{'DefaultValueSpec(?)'}->[0]) : ())
                , (@{ $item{'Constraint(s?)'} } ?
                        (p_constraints => [ @{ $item{'Constraint(s?)'} } ]) : ())
                , p_hasAccess => $hasAccess
                , p_isRef     => $isRef
                , p_isContext => $isContext
                , p_isLazy    => $isLazy
                , p_slots     => { map { $_ => 1 } @slots }
                        # "is foo<42>" not supported yet.
                );
          my $optionality = $item{'OptionalityModifier(?)'}->[0] || '';
          my $optional = 1 == @{ $item{'DefaultValueSpec(?)'} };
          die "required parameter can't have default value" if
              $optional && $optionality eq '!';
          $optional = 1 if $style eq 'named' && $optionality ne '!';
          $optional = 1 if $optionality eq '?';

          $return = { param    => $param
                    , required => !$optional
                    , style    => $style
                    };
        }

    ParamType: /[a-zA-Z]\w+/

    ParamIdentifier: ParamIdentifier_positional
                   | ParamIdentifier_named
    
    # Perl 6 allows placeholder parameters, e.g. :($) - sub of arity 1 (scalar).
    ParamIdentifier_positional: Sigil <skip:''> Label(?)
        {
          my $label = @{ $item{'Label(?)'} } ? $item{'Label(?)'}->[0] : '';
          $return = { variable => $item{Sigil} . $label
                    , label    => $label
                    , style    => 'positional'
                    };
        }

    # TODO: L<S06/"Multiple name wrappings may be given">, whoa.
    ParamIdentifier_named:
            ':' <skip:''> Label ParamIdentifier_named_variablename
        {
            $return = { variable => $item{'ParamIdentifier_named_variablename'}
                      , label => $item{Label}
                      , style => 'named'
                      };
        }
            | ':' <skip:''> Sigil Label
        {
            $return = { variable => $item{Sigil} . $item{Label}
                      , label => $item{Label}
                      , style => 'named'
                      };
        }

    ParamIdentifier_named_variablename: '(' <perl_variable> ')'
        { $return = $item[2]; 1; }

    OptionalityModifier: /[!?]/

    Unpacking: Sig

    Constraint: 'where' <perl_codeblock>

    # default values are _unevaluated_.
    DefaultValueSpec: '=' ValueOrSomeStabAtOne

    ValueOrSomeStabAtOne: Value_numberLiteral
                        | Value_acceptableQuotelike
                        | Value_variable
                        | Value_looksBalanced
                        | Value_looksClosure

    # perlfaq4 ftw
    Value_numberLiteral: /([+-]?)(?=\d|\.\d)\d*(\.\d*)?([Ee]([+-]?\d+))?/ # float
                       | /-?(?:\d+(?:\.\d*)?|\.\d+)/                      # decimal
                       | /-?\d+\.?\d*/                                    # real
                       | /[+-]?\d+/                                       # +/- integer
                       | /-?\d+/                                          # integer
                       | /\d+/                                            # whole number
                       | /0x[0-9a-fA-F]+/                                 # hexadecimal
                       | /0b[01]+/                                        # binary
                       # note that octals will be captured by the "whole number"
                       # production. Our consumer will have to eval this (we don't
                       # want to do it for them because of roundtripping. But maybe
                       # we need annotation nodes anyway?

    Value_acceptableQuotelike: <perl_quotelike>
        {
            my $op = $item[1]->[0];  # q, qq etc.
            my %whitelist = map { $_ => 1 } qw(q qq qw qr);  # TODO: lift this up
            die "rejected quotelike operator: $op" unless $whitelist{$op};
            $return = join "", @{ $item[0] };
            1;
        }

    Value_variable: <perl_variable>

    Value_looksBalanced: { extract_bracketed($text, '()') }
                       | { extract_bracketed($text, '[]') }
                       | { extract_bracketed($text, '{}') }

    Value_looksClosure: 'sub' <perl_codeblock>

    Attrib: 'is' Label

    Sigil: /[\$\@\%]/

    Label: /\w+/

    # This one is a bummer: we don't want to provide a full parser for
    # Perl expressions here. If we are called in the context of Devel::Declare,
    # perhaps we can get a reference back to the real parser? Otherwise, we're
    # stuck with doing some half-assed parsing that would preclude e.g.
    # :($pi = 22/7)
    Literal: /\S+/
.

my $parser = Parse::RecDescent->new($SIGNATURE_GRAMMAR);

sub parse {
    my($self, $sig_str) = @_;

    my $res = $parser->Sig($sig_str);

    return $res;
}

# These are my favorite debugging tools. Share and enjoy.
sub ::Y  { require YAML::Syck; YAML::Syck::Dump(@_) }
sub ::YY { require Carp; Carp::confess(::Y(@_)) }

6;

