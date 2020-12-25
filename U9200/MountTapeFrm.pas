unit MountTapeFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TMountTapeForm = class(TForm)
    Label1: TLabel;
    FileNameEdt: TEdit;
    BrowseBtn: TButton;
    WriteEnableCheck: TCheckBox;
    MountBtn: TButton;
    CancelBtn: TButton;
    OpenDlg: TOpenDialog;
    procedure BrowseBtnClick(Sender: TObject);
    procedure MountBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function GetFileName: String;
    function GeWriteEnable: Boolean;
    { Private declarations }
  public
    property FileName: String read GetFileName;
    property WriteEnable: Boolean read GeWriteEnable;
  end;

var
  MountTapeForm: TMountTapeForm;

implementation

{$R *.dfm}

procedure TMountTapeForm.BrowseBtnClick(Sender: TObject);
begin
    if (OpenDlg.Execute) then
        FileNameEdt.Text := OpenDlg.FileName;
end;

procedure TMountTapeForm.FormShow(Sender: TObject);
begin
    FileNameEdt.Text := '';
    FileNameEdt.SetFocus;
    WriteEnableCheck.Checked := True;
end;

function TMountTapeForm.GetFileName: String;
begin
    Result := FileNameEdt.Text;
end;

function TMountTapeForm.GeWriteEnable: Boolean;
begin
    Result := WriteEnableCheck.Checked;
end;

procedure TMountTapeForm.MountBtnClick(Sender: TObject);
var
    stat: Integer;
begin
    FileNameEdt.Text := Trim(FileNameEdt.Text);
    if (FileNameEdt.Text = '') then
        raise Exception.Create('Please enter the file name');
    if (not FileExists(FileNameEdt.Text)) then
    begin
        stat := MessageBox(Handle,
                           PChar(Format('File %s does not exist. Do you want to create an empty tape file?',
                                        [ExtractFileName(FileNameEdt.Text)])),
                           '',
                           MB_YESNO or MB_ICONQUESTION or MB_APPLMODAL);
        if (stat <> IDYES) then
            Exit;
    end;
    ModalResult := MrOk;
end;

end.
