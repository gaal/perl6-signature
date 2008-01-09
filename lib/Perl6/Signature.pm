package Perl6::Signature;

use Parse::RecDescent;
use Perl6::Signature::Val;

#$::RD_TRACE = 1;
$::RD_HINT  = 1;

our $SIGNATURE_GRAMMAR = << '.';
    Sig: Sig_colon | Sig_nocolon

    Sig_colon: ':' Sig_nocolon

    Sig_nocolon: '(' Sigbody ')'
        { $item{Sigbody} }

    Sigbody: Sigbody_inv | Sigbody_noinv

    #Invocant: 'THIS_SHOULD_NEVER_MATCH' # predeclaration to silence warning

    Sigbody_inv: Invocant <skip:'\s*'> ':' <commit> Sigbody_noinv
        {
          my $sig = $item{Sigbody_noinv};
          $sig->s_invocant( $item{Invocant}->{param} );
          $return = $sig;
        }

    Sigbody_noinv: Param(s? /,/)
        {
          # calculate requiredPositionalCount, and make sure we don't have
          # :($optional?, $required!) -- that's invalid.

          my $params = $item{'Param(s?)'};
          my @positionals = grep { $_->{style} } @$params;
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
              );
          $return = $sig;
        }

    Invocant: Param
        {
          $return = $item{Param};
          # die "invocant cannot be optional" unless $return->{required};
          1;
        }

    # positionals only for now
    Param: PositionalParam

    PositionalParam: ParamType(s?)
                     Identifier <skip:''>
                     OptionalityModifier(?)
                     <skip:'\s*'>
                     Unpacking(?)
                     DefaultValueSpec(?)
                     Attrib(s?)
        {
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
                ( p_types     => $item{ParamType} || []
                , p_variable  => $item{Identifier}->{sigil} .
                                 $item{Identifier}->{label}
                , ($item{'Unpacking(?)'} ? (p_unpacking => $item{'Unpacking(?)'}->[0]) : ())
                , (@{ $item{'DefaultValueSpec(?)'} } ?
                        (p_default => $item{'DefaultValueSpec(?)'}->[0]) : ())
                , p_hasAccess => $hasAccess
                , p_isRef     => $isRef
                , p_isContext => $isContext
                , p_isLazy    => $isLazy
                , p_slots     => { map { $_ => 1 } @slots }
                        # "is foo<42>" not supported yet.
                );
          my $optional = @{ $item{'DefaultValueSpec(?)'} } ||
               $item{'OptionalityModifier(?)'}->[0] eq '?';
          $return = { param    => $param
                    , required => !$optional
                    , style    => 'positional'
                    };
        }

    ParamType: /[a-zA-Z]\w+/

    # Perl 6 allows placeholder parameters, e.g. :($) - sub of arity 1 (scalar).
    Identifier: Sigil <skip:''> Label #(?)
        { $return = { sigil => $item{Sigil}, label => $item{Label} } }

    OptionalityModifier: /[!?]/

    Unpacking: Sig

    DefaultValueSpec: '=' Literal
        # or a prior arg :( or even a global, or some expression

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

