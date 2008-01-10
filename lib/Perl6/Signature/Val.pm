package Perl6::Signature::Val;

use Moose 0.33;

# Signature AST base. Add debug methods here?

package Perl6::Signature::Val::Sig;

=begin Pugscode

|-- AST for function signature.

data Sig = MkSig
    { s_invocant                  :: Maybe Param
    , s_requiredPositionalCount   :: Int
    , s_requiredNames             :: Set ID
    , s_positionalList            :: [Param]
    , s_namedSet                  :: Map.Map ID Param
    , s_slurpyScalarList          :: [Param]
    , s_slurpyArray               :: Maybe Param
    , s_slurpyHash                :: Maybe Param
    , s_slurpyCode                :: Maybe Param
    , s_slurpyCapture             :: Maybe Param
    }

=cut

# XXX: L<S06/"Bindings happen in declaration order, not call order">
# suggests that requiredNames should be modeled with a list, not a set,
# and that the compiler convert mandatory named args to positionals.
# Fix this sometime.

use Moose;
extends 'Perl6::Signature::Val';

has 's_invocant' =>
    (is => 'rw', isa => 'Perl6::Signature::Val::SigParam', required => 0);
has 's_requiredPositionalCount' =>
    (is => 'rw', isa => 'Int');
has 's_requiredNames' =>
    (is => 'rw', isa => 'HashRef');   # Set of names
has 's_positionalList' =>
    (is => 'rw', isa => 'ArrayRef[Perl6::Signature::Val::SigParam]');
has 's_namedList' =>
    (is => 'rw', isa => 'ArrayRef[Perl6::Signature::Val::SigParam]');
has 's_slurpyScalarList' =>
    (is => 'rw', isa => 'ArrayRef', required => 0);
has 's_slurpyArray' =>
    (is => 'rw', isa => 'Perl6::Signature::Val::SigParam', required => 0);
has 's_slurpyHash' =>
    (is => 'rw', isa => 'Perl6::Signature::Val::SigParam', required => 0);
has 's_slurpyCode' =>
    (is => 'rw', isa => 'Perl6::Signature::Val::SigParam', required => 0);
has 's_slurpyCapture' =>
    (is => 'rw', isa => 'Perl6::Signature::Val::SigParam', required => 0);

sub find_named_param {
    my($self, $label) = @_;
    for my $param (@{ $self->s_namedList }) {
        return $param if $param->p_label eq $label;
    }
    return;  # Or should this die?
}

sub to_string {
    my($self) = @_;

    my $inv_str;
    if (my $inv = $self->s_invocant) {
        $inv_str = $inv->to_string . ":";
    }

    my @params;
    my $positionals = $self->s_positionalList;
    for my $i (0 .. $#$positionals) {
        push @params, $positionals->[$i]->to_string(
                required => $i < $self->s_requiredPositionalCount);
    }
    push @params, map {
        $_->to_string( style => 'named'
                     , required => exists $self->s_requiredNames->{$_->p_label})
        } @{ $self->s_namedList };

    return ":(" .
            join(" ", ($inv_str ? $inv_str : ()),
                (@params ? join(", ", @params) : ())) .
           ")";
}

package Perl6::Signature::Val::SigParam;

use Moose;
extends 'Perl6::Signature::Val';

=begin Pugscode

-- | Single parameter for a function or method, e.g.:
--   Elk $m where { $m.antlers ~~ Velvet }
{-|
A formal parameter of a sub (or other callable).

These represent declared parameters; don't confuse them with actual argument
values.
-}
data SigParam = MkParam
    { p_variable    :: Var           -- ^ E.g. $m above
    , p_types       :: [Types.Type]  -- ^ Static pieces of inferencer-food
                                     --   E.g. Elk above
    , p_constraints :: [Code]        -- ^ Dynamic pieces of runtime-mood
                                     --   E.g. where {...} above
    , p_unpacking   :: Maybe PureSig -- ^ E.g. BinTree $t (Left $l, Right $r)
    , p_default     :: ParamDefault  -- ^ E.g. $answer? = 42
    , p_label       :: ID            -- ^ The external name for the param ('m' above)
    , p_slots       :: Table         -- ^ Any additional attrib not
                                     --   explicitly mentioned below
    , p_hasAccess   :: ParamAccess   -- ^ is ro, is rw, is copy
    , p_isRef       :: Bool          -- ^ must be true if hasAccess = AccessRW
    , p_isContext   :: Bool          -- ^ "is context"
    , p_isLazy      :: Bool
    }

=cut

has 'p_variable' =>    (is => 'rw', isa => 'Str');
has 'p_types' =>       (is => 'rw', isa => 'ArrayRef');  # of types
        # I don't actually remember why this isn't a scalar :(
has 'p_constraints' => (is => 'rw', isa => 'ArrayRef');  # of code
has 'p_unpacking' =>   (is => 'rw', isa => 'Perl6::Signature::Val::Sig|Undef',
                        required => 0);
has 'p_default' =>     (is => 'rw', required => 0);
has 'p_label' =>       (is => 'rw', isa => 'Str');
has 'p_slots' =>       (is => 'rw', isa => 'HashRef');
has 'p_hasAccess' =>   (is => 'rw', );                   # XXX: enum?
has 'p_isRef' =>       (is => 'rw', isa => 'Bool');
has 'p_isContext' =>   (is => 'rw', isa => 'Bool');
has 'p_isLazy' =>      (is => 'rw', isa => 'Bool');

my %quoted_slots = map { $_ => 1 } qw(ro rw copy ref context lazy);

sub to_string {
    my($self, %args) = @_;

    $args{required} = 1            if not exists $args{required};
    $args{style}    = 'positional' if not exists $args{style};

    die "required param can't have a default value" if
            $args{required} && $self->p_default;

    my $ident;
    if ($args{style} eq 'positional') {
        $ident = $self->p_variable;
        $ident .= "?" if !$args{required} && not defined $self->p_default;
    } else {
        # TODO: implement a Perl6::...::Variable::basename
        my($label, $variable) = ($self->p_label, $self->p_variable);
        $ident = ":" . (($variable =~ /^.\Q$label\E$/) ?
            $variable : "$label($variable)");
        $ident .= "!" if $args{required};
    }

    my $default = "= " . $self->p_default if $self->p_default;

    my $p_slots = $self->p_slots || {};
    my @slots;
    $self->p_hasAccess('ro') if ! $self->p_hasAccess;
    push @slots, "is " . $self->p_hasAccess if $self->p_hasAccess ne 'ro';
    push @slots, "is ref"     if $self->p_isRef;
    push @slots, "is context" if $self->p_isContext;
    push @slots, "is lazy"    if $self->p_isLazy;
    push @slots, map {
        my $qkey = $quoted_slots{$_} ? "'$_'" : $_;
        my $val  = defined $p_slots->{$_} && $p_slots->{$_} == 1 ?
            "" : "<$p_slots->{$_}>";
        "is $qkey$val" } keys %$p_slots;

    my @constraints = map { "where $_" } @{ $self->p_constraints || [] };

    return join(" ", @{ $self->p_types },
            $ident,
            ($self->p_unpacking ? $self->p_unpacking->to_string : ()),
            ($default ? $default : ()),
            @slots,
            @constraints);
}

6;

