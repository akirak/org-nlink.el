{
  description =
    "THIS IS AN AUTO-GENERATED FILE. PLEASE DON'T EDIT IT MANUALLY.";
  inputs = {
    buttercup = {
      flake = false;
      owner = "jorgenschaefer";
      repo = "emacs-buttercup";
      type = "github";
    };
    compat = {
      flake = false;
      owner = "emacs-compat";
      repo = "compat";
      type = "github";
    };
    consult = {
      flake = false;
      owner = "minad";
      repo = "consult";
      type = "github";
    };
  };
  outputs = { ... }: { };
}
