{
  outputs = {...}: {
    elisp-rice = {
      packages = [
        "org-nlink"
        "consult-org-nlink"
      ];
      tests = {
        buttercup.enable = true;
      };
    };
  };
}
